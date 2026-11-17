use std::fs;
use std::path::Path;

use assert_cmd::Command;
use predicates::prelude::*;
use tempfile::TempDir;

const SELF_ASSIGNMENT: &str = "class Foo extends Object;\nfunction F(int A)\n{\n    A = A;\n}\n";
const CLEAN: &str = "class Bar extends Object;\n";

fn project(files: &[(&str, &str)]) -> TempDir {
    let dir = TempDir::new().unwrap();
    for (path, contents) in files {
        let path = dir.path().join(path);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, contents).unwrap();
    }
    dir
}

fn usagi(dir: &Path) -> Command {
    let mut command = Command::cargo_bin("usagi").unwrap();
    command.current_dir(dir).env_remove("NO_COLOR");
    command
}

fn concise(dir: &Path, args: &[&str]) -> String {
    let output = usagi(dir)
        .args(["lint", "--format", "concise"])
        .args(args)
        .output()
        .unwrap();
    String::from_utf8(output.stdout).unwrap()
}

#[test]
fn exit_codes() {
    let dir = project(&[("A/Foo.uc", SELF_ASSIGNMENT), ("B/Bar.uc", CLEAN)]);
    usagi(dir.path()).args(["lint", "B"]).assert().code(0);
    usagi(dir.path()).args(["lint"]).assert().code(1);
    usagi(dir.path()).args(["lint", "Missing"]).assert().code(2);
    usagi(dir.path())
        .args(["lint", "--select", "nonsense"])
        .assert()
        .code(2)
        .stderr(predicate::str::contains("unknown lint \"nonsense\""));
    usagi(dir.path()).args(["lint", "--bogus"]).assert().code(2);
    usagi(dir.path()).args(["format"]).assert().code(2);
}

#[test]
fn full_output_shows_the_source() {
    let dir = project(&[("Foo.uc", SELF_ASSIGNMENT)]);
    usagi(dir.path())
        .args(["lint", "--color", "never"])
        .assert()
        .code(1)
        .stdout(predicate::str::contains("warning[S003]"))
        .stdout(predicate::str::contains("Foo.uc:4:5"))
        .stdout(predicate::str::contains("    A = A;"));
}

#[test]
fn json_output() {
    let dir = project(&[("Foo.uc", SELF_ASSIGNMENT)]);
    let output = usagi(dir.path())
        .args(["lint", "--format", "json"])
        .output()
        .unwrap();
    let problems: serde_json::Value = serde_json::from_slice(&output.stdout).unwrap();
    assert_eq!(
        problems,
        serde_json::json!([{
            "path": "Foo.uc",
            "line": 4,
            "column": 5,
            "end_line": 4,
            "end_column": 10,
            "code": "S003",
            "name": "self-assignment",
            "severity": "warning",
            "message": "Variable is assigned to itself",
        }])
    );
}

#[test]
fn output_is_sorted_by_path() {
    let files: Vec<(String, &str)> = (0..20)
        .map(|i| (format!("P{:02}/Foo.uc", 19 - i), SELF_ASSIGNMENT))
        .collect();
    let files: Vec<(&str, &str)> = files.iter().map(|(p, c)| (p.as_str(), *c)).collect();
    let dir = project(&files);
    let output = concise(dir.path(), &[]);
    let paths: Vec<&str> = output.lines().map(|l| &l[..3]).collect();
    let mut sorted = paths.clone();
    sorted.sort();
    assert_eq!(paths, sorted);
    assert_eq!(paths.len(), 20);
}

#[test]
fn config_file() {
    let dir = project(&[
        ("usagi.toml", "[lint]\nignore = [\"S003\"]\n"),
        ("Foo.uc", SELF_ASSIGNMENT),
    ]);
    usagi(dir.path()).arg("lint").assert().code(0);
    // Found from a subdirectory too.
    fs::create_dir(dir.path().join("Sub")).unwrap();
    usagi(&dir.path().join("Sub"))
        .args(["lint", ".."])
        .assert()
        .code(0);
    // Flags override it.
    assert!(concise(dir.path(), &["--select", "S003"]).is_empty());
    assert!(concise(dir.path(), &["--no-config"]).contains("S003"));
}

#[test]
fn select_and_extend_select() {
    let dir = project(&[
        ("usagi.toml", "[lint]\nselect = [\"naming\"]\n"),
        ("Foo.uc", SELF_ASSIGNMENT),
    ]);
    assert!(concise(dir.path(), &[]).is_empty());
    assert!(concise(dir.path(), &["--extend-select", "S003"]).contains("S003"));
    assert!(concise(dir.path(), &["--select", "suspicious"]).contains("S003"));
}

#[test]
fn per_path_settings() {
    let config = "[lint]\nselect = [\"S003\"]\n\n\
                  [[lint.per-path]]\npaths = [\"Legacy\"]\nignore = [\"S003\"]\n";
    let dir = project(&[
        ("usagi.toml", config),
        ("Legacy/Classes/Foo.uc", SELF_ASSIGNMENT),
        ("New/Classes/Foo.uc", SELF_ASSIGNMENT),
    ]);
    let output = concise(dir.path(), &[]);
    assert!(
        output.starts_with("New/Classes/Foo.uc:4:5: S003"),
        "{}",
        output
    );
    assert_eq!(output.lines().count(), 1);
}

#[test]
fn include_and_exclude() {
    let dir = project(&[
        (
            "usagi.toml",
            "include = [\"Pkg*\"]\nexclude = [\"PkgB/**/Skip*.uc\"]\n",
        ),
        ("PkgA/Classes/Foo.uc", SELF_ASSIGNMENT),
        ("PkgB/Classes/SkipMe.uc", SELF_ASSIGNMENT),
        ("PkgB/Classes/Keep.uc", SELF_ASSIGNMENT),
        ("Other/Classes/Foo.uc", SELF_ASSIGNMENT),
    ]);
    let output = concise(dir.path(), &[]);
    assert!(output.contains("PkgA/Classes/Foo.uc"));
    assert!(output.contains("PkgB/Classes/Keep.uc"));
    assert!(!output.contains("SkipMe"));
    assert!(!output.contains("Other"));
    // Flags replace the configured lists.
    let output = concise(dir.path(), &["--include", "other"]);
    assert_eq!(output.lines().count(), 1, "{}", output);
    // A file named directly is always checked.
    let output = concise(dir.path(), &["Other/Classes/Foo.uc"]);
    assert_eq!(output.lines().count(), 1, "{}", output);
}

#[test]
fn invalid_config() {
    let dir = project(&[("usagi.toml", "colour = true\n"), ("Foo.uc", CLEAN)]);
    usagi(dir.path())
        .arg("lint")
        .assert()
        .code(2)
        .stderr(predicate::str::contains("usagi.toml"))
        .stderr(predicate::str::contains("colour"));
    let dir = project(&[
        ("usagi.toml", "[lint]\nignore = [\"X999\"]\n"),
        ("Foo.uc", CLEAN),
    ]);
    usagi(dir.path())
        .arg("lint")
        .assert()
        .code(2)
        .stderr(predicate::str::contains("lint.ignore"));
}

#[test]
fn parse_outcomes() {
    let recovered = "class Foo extends Object;\ndefaultproperties\n{\n    0.905471\n}\n";
    let dir = project(&[
        ("Good.uc", CLEAN),
        ("Recovered.uc", recovered),
        ("Broken.uc", "class Foo extends Object;\nfunction {\n"),
    ]);
    usagi(dir.path())
        .args([
            "parse",
            "--statistics",
            "--format",
            "concise",
            "Good.uc",
            "Recovered.uc",
        ])
        .assert()
        .code(0)
        .stdout(predicate::str::contains("Recovered.uc:4:5: parse"))
        .stdout(predicate::str::contains("recovered"));
    usagi(dir.path())
        .args(["parse", "--strict", "Recovered.uc"])
        .assert()
        .code(1);
    usagi(dir.path())
        .args(["parse", "--format", "concise", "Broken.uc"])
        .assert()
        .code(1)
        .stdout(predicate::str::contains("Broken.uc:2:10: syntax"));
}

#[test]
fn parse_ast() {
    let dir = project(&[("Bar.uc", CLEAN)]);
    usagi(dir.path())
        .args(["parse", "--ast"])
        .assert()
        .code(0)
        .stdout(predicate::str::contains("ClassDeclaration"));
}

#[test]
fn windows_1252_source() {
    let dir = project(&[]);
    fs::write(
        dir.path().join("Foo.uc"),
        b"class Foo extends Object;\n// caf\xE9\nfunction F(int A) { A = A; }\n",
    )
    .unwrap();
    assert!(concise(dir.path(), &[]).contains("Foo.uc:3:21: S003"));
}

#[test]
fn rule() {
    let dir = project(&[]);
    usagi(dir.path())
        .args(["rule", "self-assignment"])
        .assert()
        .code(0)
        .stdout(predicate::str::starts_with("S003 self-assignment"));
    usagi(dir.path())
        .args(["rule", "--all"])
        .assert()
        .code(0)
        .stdout(predicate::str::contains("R013"));
    usagi(dir.path()).args(["rule", "Z999"]).assert().code(2);
}
