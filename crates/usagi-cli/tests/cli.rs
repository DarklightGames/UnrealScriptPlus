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
            "fix": null,
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

#[test]
fn utf8_source() {
    let dir = project(&[("Foo.uc", "class Foo extends Object;\n// G\u{fc}nther\n")]);
    usagi(dir.path())
        .args(["parse", "--format", "concise"])
        .assert()
        .code(0)
        .stdout(predicate::str::contains(
            "Foo.uc:2:5: parse The file is UTF-8",
        ));
    usagi(dir.path())
        .args(["parse", "--strict"])
        .assert()
        .code(1);
}

const FIXABLE: &[u8] = b"class Foo extends Actor;\r\nvar int I;\r\nfunction F(int A)\r\n{\r\n    // caf\xE9\r\n    Self.I = int(True);\r\n    A = A;\r\n    return;\r\n}\r\n";
const FIXED: &[u8] = b"class Foo extends Actor;\r\nvar int I;\r\nfunction F(int A)\r\n{\r\n    // caf\xE9\r\n    I = int(true);\r\n    A = A;\r\n}\r\n";

fn fixable_project() -> TempDir {
    let dir = project(&[]);
    fs::write(dir.path().join("Foo.uc"), FIXABLE).unwrap();
    dir
}

#[test]
fn fixable_problems_are_marked() {
    let dir = fixable_project();
    let output = concise(dir.path(), &[]);
    assert!(
        output.contains("Foo.uc:6:5: R012 [*] Redundant"),
        "{}",
        output
    );
    assert!(output.contains("Foo.uc:7:5: S003 Variable"), "{}", output);
    usagi(dir.path())
        .args(["lint", "--color", "never"])
        .assert()
        .stdout(predicate::str::contains(
            "Remove `Self.` (fixable with --fix)",
        ))
        .stderr(predicate::str::contains("3 problems can be fixed"));
    let output = usagi(dir.path())
        .args(["lint", "--format", "json", "--select", "R013"])
        .output()
        .unwrap();
    let problems: serde_json::Value = serde_json::from_slice(&output.stdout).unwrap();
    assert_eq!(problems[0]["fix"], "Remove `return;`");
}

#[test]
fn diff_changes_nothing() {
    let dir = fixable_project();
    usagi(dir.path())
        .args(["lint", "--diff"])
        .assert()
        .code(1)
        .stdout(predicate::str::contains("--- a/Foo.uc"))
        .stdout(predicate::str::contains("-    Self.I = int(True);"))
        .stdout(predicate::str::contains("+    I = int(true);"))
        .stderr(predicate::str::contains("Would fix 3 problems in 1 file"));
    assert_eq!(fs::read(dir.path().join("Foo.uc")).unwrap(), FIXABLE);
    // Nothing to fix.
    let dir = project(&[("Bar.uc", CLEAN)]);
    usagi(dir.path())
        .args(["lint", "--diff"])
        .assert()
        .code(0)
        .stdout("");
    usagi(dir.path())
        .args(["lint", "--diff", "--fix"])
        .assert()
        .code(2);
}

#[test]
fn fix_writes_files_in_their_encoding() {
    let dir = fixable_project();
    usagi(dir.path())
        .args(["lint", "--fix", "--format", "concise"])
        .assert()
        .code(1)
        .stdout(predicate::str::contains("S003"))
        .stdout(predicate::str::contains("R012").not())
        .stderr(predicate::str::contains("Fixed 3 problems in 1 file"));
    assert_eq!(fs::read(dir.path().join("Foo.uc")).unwrap(), FIXED);
    // Fixing again changes nothing.
    usagi(dir.path())
        .args(["lint", "--fix", "--ignore", "S003"])
        .assert()
        .code(0)
        .stderr(predicate::str::contains("Fixed 0 problems in 0 files"));
    assert_eq!(fs::read(dir.path().join("Foo.uc")).unwrap(), FIXED);
}

#[test]
fn fix_respects_the_selection() {
    let dir = fixable_project();
    usagi(dir.path())
        .args(["lint", "--fix", "--select", "F001"])
        .assert()
        .code(0);
    let fixed = fs::read(dir.path().join("Foo.uc")).unwrap();
    let fixed = String::from_utf8_lossy(&fixed);
    assert!(fixed.contains("Self.I = int(true);"), "{}", fixed);
    assert!(fixed.contains("return;"), "{}", fixed);
}

#[test]
fn fix_keeps_utf16() {
    let dir = project(&[]);
    let source = "class Foo extends Object;\nfunction F() { G(True); }\n";
    let utf16: Vec<u8> = [0xFF, 0xFE]
        .into_iter()
        .chain(source.encode_utf16().flat_map(u16::to_le_bytes))
        .collect();
    fs::write(dir.path().join("Foo.uc"), utf16).unwrap();
    usagi(dir.path())
        .args(["lint", "--fix", "--select", "F001"])
        .assert()
        .code(0);
    let expected: Vec<u8> = [0xFF, 0xFE]
        .into_iter()
        .chain(
            source
                .replace("True", "true")
                .encode_utf16()
                .flat_map(u16::to_le_bytes),
        )
        .collect();
    assert_eq!(fs::read(dir.path().join("Foo.uc")).unwrap(), expected);
}

/// A fake UCC, run by `sh` in place of Wine. `make` "compiles" each of the
/// mod's packages that isn't in its System folder, unless the package has a
/// `Broken.uc`, and logs what it compiled to `made.log`. `dumpint` writes the
/// package's `.int`. Both overwrite `UCC.log`.
#[cfg(unix)]
const FAKE_UCC: &str = r#"mod=${2#-mod=}
if [ "$1" = Editor.MakeCommandlet ]; then
  mod=${2#-mod=}
  echo "Parsing everything"
  for p in $(sed -n 's/^+EditPackages=//p' ../$mod/System/Default.ini | tr -d '\r'); do
    [ -f ../$mod/System/$p.u ] && continue
    if [ -e ../$p/Classes/Broken.uc ]; then
      echo "$p.uc(1) : Error, broken"
    else
      echo "$p" > $p.u
      echo "$p" >> ../made.log
    fi
  done
  echo make > UCC.log
else
  mod=${3#-mod=}
  p=${2%.u}
  echo "No packages found matching Editor.DumpIntCommandlet!"
  echo "[Public]" > $p.int
  echo "Failure - 1 error(s), 0 warning(s)"
  echo dumpint > UCC.log
  exit 1
fi
"#;

#[cfg(unix)]
fn game() -> TempDir {
    let default_ini =
        "[Editor.EditorEngine]\r\n+EditPackages=A\r\n+EditPackages=B\r\n+EditPackages=C\r\n";
    project(&[
        ("usagi.toml", "[build]\nmod = \"Mod\"\nwine = \"sh\"\n"),
        ("System/UCC.exe", FAKE_UCC),
        ("System/Core.u", ""),
        (
            "System/Default.ini",
            "[Editor.EditorEngine]\nEditPackages=Core\n",
        ),
        ("Mod/System/Default.ini", default_ini),
        ("A/Classes/A.uc", "class A extends Object;\n"),
        ("B/Classes/B.uc", "class B extends A;\n"),
        ("C/Classes/C.uc", "class C extends B;\n"),
    ])
}

#[cfg(unix)]
fn build(dir: &Path, args: &[&str]) -> (i32, String) {
    let output = usagi(dir)
        .args(["build", "--color", "never"])
        .args(args)
        .output()
        .unwrap();
    let stdout = String::from_utf8(output.stdout).unwrap();
    let stderr = String::from_utf8(output.stderr).unwrap();
    (output.status.code().unwrap(), stdout + &stderr)
}

/// What the fake UCC compiled since the last call.
#[cfg(unix)]
fn made(dir: &Path) -> String {
    let path = dir.join("made.log");
    let made = fs::read_to_string(&path).unwrap_or_default();
    let _ = fs::remove_file(path);
    made
}

#[cfg(unix)]
#[test]
fn build_compiles_only_what_changed() {
    let dir = game();
    let root = dir.path();
    let (code, out) = build(root, &[]);
    assert_eq!(code, 0, "{}", out);
    assert!(out.contains("Default.ini changed"), "{}", out);
    assert!(out.contains("3 succeeded, 0 failed"), "{}", out);
    assert!(!out.contains("Parsing everything"), "{}", out);
    assert!(
        out.contains("Exported localization: 3 succeeded"),
        "{}",
        out
    );
    assert_eq!(made(root), "A\nB\nC\n");
    for file in ["A.u", "B.u", "C.u", "A.int"] {
        assert!(root.join("Mod/System").join(file).is_file(), "{}", file);
        assert!(!root.join("System").join(file).exists(), "{}", file);
    }
    // UCC.log is the make's, not the last dumpint's.
    assert_eq!(
        fs::read_to_string(root.join("System/UCC.log")).unwrap(),
        "make\n"
    );
    assert!(root.join("Mod/.usagi-build.toml").is_file());

    let (code, out) = build(root, &[]);
    assert_eq!(code, 0, "{}", out);
    assert!(
        out.contains("0 succeeded, 0 failed, 0 skipped, 3 up-to-date"),
        "{}",
        out
    );
    assert_eq!(made(root), "");

    // A change rebuilds the package and the ones after it...
    fs::write(
        root.join("B/Classes/B.uc"),
        "class B extends A; // changed\n",
    )
    .unwrap();
    let (code, out) = build(root, &["--verbose"]);
    assert_eq!(code, 0, "{}", out);
    assert!(out.contains("Parsing everything"), "{}", out);
    assert!(out.contains("B: changed"), "{}", out);
    assert!(
        out.contains("C: rebuilding because a dependency changed"),
        "{}",
        out
    );
    assert!(!out.contains("Exported localization"), "{}", out);
    assert_eq!(made(root), "B\nC\n");

    // ...unless dependencies are ignored. Adding a file is a change too.
    fs::write(root.join("B/Classes/B2.uc"), "class B2 extends B;\n").unwrap();
    let (code, out) = build(root, &["--ignore-dependencies"]);
    assert_eq!(code, 0, "{}", out);
    assert_eq!(made(root), "B\n");

    let (code, out) = build(root, &["--clean"]);
    assert_eq!(code, 0, "{}", out);
    assert_eq!(made(root), "A\nB\nC\n");
}

#[cfg(unix)]
#[test]
fn build_retries_packages_that_failed() {
    let dir = game();
    let root = dir.path();
    build(root, &[]);
    made(root);
    fs::write(
        root.join("A/Classes/Broken.uc"),
        "class Broken extends Object;\n",
    )
    .unwrap();
    let (code, out) = build(root, &[]);
    assert_eq!(code, 1, "{}", out);
    assert!(out.contains("A.uc(1) : Error, broken"), "{}", out);
    assert!(out.contains("2 succeeded, 1 failed"), "{}", out);
    assert_eq!(made(root), "B\nC\n");

    // A was the only change, but B and C depend on it, so they're rebuilt
    // too, even though they compiled last time.
    fs::remove_file(root.join("A/Classes/Broken.uc")).unwrap();
    let (code, out) = build(root, &[]);
    assert_eq!(code, 0, "{}", out);
    assert_eq!(made(root), "A\nB\nC\n");
}

#[cfg(unix)]
#[test]
fn build_stops_on_syntax_errors() {
    let dir = game();
    let root = dir.path();
    fs::write(
        root.join("B/Classes/B.uc"),
        "class B extends A;\nfunction {\n",
    )
    .unwrap();
    let (code, out) = build(root, &[]);
    assert_eq!(code, 1, "{}", out);
    assert!(out.contains("1 file didn't parse"), "{}", out);
    assert!(out.contains("B.uc"), "{}", out);
    assert_eq!(made(root), "");
    let (code, out) = build(root, &["--no-check"]);
    assert_eq!(code, 0, "{}", out);
    assert_eq!(made(root), "A\nB\nC\n");
}

#[cfg(unix)]
#[test]
fn build_updates_the_mod_config_and_skips_game_packages() {
    let dir = game();
    let root = dir.path();
    fs::write(
        root.join("Mod/System/Mod.ini"),
        "[Editor.EditorEngine]\r\nGridSize=7\r\nEditPackages=Core\r\nEditPackages=Old\r\n",
    )
    .unwrap();
    let (code, out) = build(root, &[]);
    assert_eq!(code, 0, "{}", out);
    assert!(
        out.contains("Updated [Editor.EditorEngine] EditPackages in Mod.ini"),
        "{}",
        out
    );
    assert_eq!(
        fs::read_to_string(root.join("Mod/System/Mod.ini")).unwrap(),
        "[Editor.EditorEngine]\r\nGridSize=7\r\nEditPackages=Core\r\nEditPackages=A\r\nEditPackages=B\r\nEditPackages=C\r\n"
    );
    // Core is compiled already, so it isn't one of the mod's packages.
    assert!(
        out.contains("3 succeeded, 0 failed, 0 skipped, 0 up-to-date"),
        "{}",
        out
    );
}

#[cfg(unix)]
#[test]
fn build_writes_the_version_class() {
    let dir = game();
    let root = dir.path();
    fs::write(
        root.join("Mod/usagi.toml"),
        "[build]\nversion = \"1.2.3-beta\"\nversion-class = \"A/Classes/AVersion.uc\"\n",
    )
    .unwrap();
    let (code, out) = build(root, &[]);
    assert_eq!(code, 0, "{}", out);
    let class = fs::read_to_string(root.join("A/Classes/AVersion.uc")).unwrap();
    assert!(
        class.contains("class AVersion extends Object;"),
        "{}",
        class
    );
    assert!(class.contains("Prerelease=\"beta\""), "{}", class);
    // Unchanged, so A isn't rebuilt.
    made(root);
    let (code, out) = build(root, &[]);
    assert_eq!(code, 0, "{}", out);
    assert_eq!(made(root), "");
}

#[test]
fn build_setup_errors() {
    let dir = project(&[("System/UCC.exe", "")]);
    usagi(dir.path())
        .args(["build"])
        .assert()
        .code(2)
        .stderr(predicate::str::contains("no mod to build"));
    usagi(dir.path())
        .args(["build", "--mod", "Missing"])
        .assert()
        .code(2)
        .stderr(predicate::str::contains("mod directory not found"));
}

#[cfg(unix)]
#[test]
fn build_reads_each_mods_own_settings() {
    let dir = game();
    let root = dir.path();
    // The mod's file wins over the game directory's for `wine`.
    fs::write(root.join("usagi.toml"), "[build]\nwine = \"false\"\n").unwrap();
    fs::write(root.join("Mod/usagi.toml"), "[build]\nwine = \"sh\"\n").unwrap();
    let (code, out) = build(root, &["--mod", "Mod"]);
    assert_eq!(code, 0, "{}", out);
    assert_eq!(made(root), "A\nB\nC\n");

    fs::write(root.join("Mod/usagi.toml"), "[build]\nroot = \"..\"\n").unwrap();
    let (code, out) = build(root, &["--mod", "Mod"]);
    assert_eq!(code, 2, "{}", out);
    assert!(
        out.contains("build.root can't be set in a mod's configuration"),
        "{}",
        out
    );

    fs::remove_file(root.join("Mod/usagi.toml")).unwrap();
    fs::write(
        root.join("usagi.toml"),
        "[build]\nmod = \"Mod\"\nversion-class = \"A/Classes/V.uc\"\n",
    )
    .unwrap();
    let (code, out) = build(root, &[]);
    assert_eq!(code, 2, "{}", out);
    assert!(
        out.contains("build.version-class can only be set in the mod's own configuration"),
        "{}",
        out
    );
}

#[test]
fn lint_reads_the_mods_settings() {
    let dir = project(&[
        (
            "usagi.toml",
            "[build]\nmod = \"Mod\"\n[lint]\nignore = [\"N\"]\n",
        ),
        (
            "Mod/usagi.toml",
            "include = [\"A*\"]\n[lint]\nextend-select = []\n",
        ),
        ("Mod/System/Default.ini", ""),
        ("A1/Classes/Foo.uc", SELF_ASSIGNMENT),
        ("B/Classes/Foo.uc", SELF_ASSIGNMENT),
    ]);
    // The mod's globs are relative to the game directory, from anywhere in it.
    let out = concise(dir.path(), &[]);
    assert!(out.contains("A1/Classes/Foo.uc"), "{}", out);
    assert!(!out.contains("B/Classes"), "{}", out);
    let out = concise(&dir.path().join("A1"), &[]);
    assert!(out.contains("Classes/Foo.uc"), "{}", out);

    // A mod's file can't say where the game is.
    fs::write(
        dir.path().join("Mod/usagi.toml"),
        "[build]\nmod = \"Other\"\n",
    )
    .unwrap();
    usagi(dir.path())
        .args(["lint"])
        .assert()
        .code(2)
        .stderr(predicate::str::contains(
            "build.mod can't be set in a mod's configuration",
        ));
}

#[test]
fn jobs_flag_and_setting() {
    let dir = project(&[("A/Foo.uc", SELF_ASSIGNMENT), ("B/Bar.uc", CLEAN)]);
    for args in [&["-j", "1"][..], &["--jobs", "3"]] {
        assert!(concise(dir.path(), args).contains("S003"));
    }
    usagi(dir.path()).args(["lint", "-j", "0"]).assert().code(2);
    fs::write(dir.path().join("usagi.toml"), "jobs = 2\n").unwrap();
    assert!(concise(dir.path(), &[]).contains("S003"));
    fs::write(dir.path().join("usagi.toml"), "jobs = 0\n").unwrap();
    usagi(dir.path()).args(["lint"]).assert().code(2);
}
