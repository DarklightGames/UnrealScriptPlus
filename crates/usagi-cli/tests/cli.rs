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

/// A fake UCC, run by `sh` in place of Wine. `make` "compiles" each package
/// that isn't compiled yet, unless it has a `Broken.uc`, and logs what it
/// compiled to `made.log`. With `-mod=`, the packages are the `+EditPackages`
/// of the mod's `Default.ini`, compiled ones are in the mod's System folder,
/// and new ones are written to `System/`; with `-ini=`, the packages are that
/// INI's `EditPackages`, all in `System/`. `dumpint` writes the package's
/// `.int`, and fails with the error that the real DumpInt reports for its own
/// name. Both overwrite `UCC.log`.
#[cfg(unix)]
const FAKE_UCC: &str = r#"cmd=$1
shift
mod=
ini=
pkg=
for a in "$@"; do
  case "$a" in
    -mod=*) mod=${a#-mod=} ;;
    -ini=*) ini=${a#-ini=} ;;
    -debug) ;;
    *) pkg=$a ;;
  esac
done
if [ "$cmd" = Editor.MakeCommandlet ]; then
  echo "Parsing everything"
  if [ -n "$mod" ]; then
    list=$(sed -n 's/^+EditPackages=//p' ../$mod/System/Default.ini | tr -d '\r')
    out=../$mod/System
  else
    list=$(sed -n 's/^EditPackages=//p' $ini | tr -d '\r')
    out=.
  fi
  for p in $list; do
    [ -f $out/$p.u ] && continue
    if [ -e ../$p/Classes/Broken.uc ]; then
      echo "$p.uc(1) : Error, broken"
    else
      echo "$p" > $p.u
      echo "$p" >> ../made.log
    fi
  done
  echo make > UCC.log
else
  p=${pkg%.u}
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
        .stderr(predicate::str::contains("no INI in"))
        .stderr(predicate::str::contains("has EditPackages"));
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

/// A game with no mod: its own packages (Core, with sources, and NoSource,
/// without) are compiled already, and A, B and C are new.
#[cfg(unix)]
fn modless_game(config: &str) -> TempDir {
    let edit_packages = "[Editor.EditorEngine]\r\nEditPackages=Core\r\nEditPackages=NoSource\r\nEditPackages=A\r\nEditPackages=B\r\nEditPackages=C\r\n";
    project(&[
        ("usagi.toml", &format!("[build]\nwine = \"sh\"\n{}", config)),
        ("System/UCC.exe", FAKE_UCC),
        ("System/Core.u", "stock"),
        ("System/NoSource.u", "stock"),
        ("System/Game.ini", edit_packages),
        // Templates and user settings aren't candidates for the INI.
        ("System/Default.ini", edit_packages),
        ("System/User.ini", edit_packages),
        ("Core/Classes/Object.uc", "class Object;\n"),
        ("A/Classes/A.uc", "class A extends Object;\n"),
        ("B/Classes/B.uc", "class B extends A;\n"),
        ("C/Classes/C.uc", "class C extends B;\n"),
    ])
}

#[cfg(unix)]
#[test]
fn modless_build_compiles_into_system() {
    let dir = modless_game("locked = [\"core\"]\n");
    let root = dir.path();
    let (code, out) = build(root, &["--dumpint"]);
    assert_eq!(code, 0, "{}", out);
    assert!(out.contains("Build started for Game.ini"), "{}", out);
    assert!(
        out.contains("3 succeeded, 0 failed, 0 skipped, 2 up-to-date"),
        "{}",
        out
    );
    assert_eq!(made(root), "A\nB\nC\n");
    for file in ["A.u", "C.u", "A.int"] {
        assert!(root.join("System").join(file).is_file(), "{}", file);
    }
    // The game's packages are untouched, and the record is in the game directory.
    assert_eq!(
        fs::read_to_string(root.join("System/Core.u")).unwrap(),
        "stock"
    );
    assert_eq!(
        fs::read_to_string(root.join("System/NoSource.u")).unwrap(),
        "stock"
    );
    assert!(root.join(".usagi-build.toml").is_file());

    let (code, out) = build(root, &[]);
    assert_eq!(code, 0, "{}", out);
    assert!(
        out.contains("0 succeeded, 0 failed, 0 skipped, 5 up-to-date"),
        "{}",
        out
    );

    fs::write(
        root.join("B/Classes/B.uc"),
        "class B extends A; // changed\n",
    )
    .unwrap();
    let (code, out) = build(root, &[]);
    assert_eq!(code, 0, "{}", out);
    assert_eq!(made(root), "B\nC\n");
}

#[cfg(unix)]
#[test]
fn locked_packages_are_never_rebuilt() {
    let dir = modless_game("locked = [\"Core\"]\n");
    let root = dir.path();
    build(root, &[]);
    made(root);

    let core = root.join("Core/Classes/Object.uc");
    fs::write(&core, "class Object; // changed\n").unwrap();
    let (code, out) = build(root, &[]);
    assert_eq!(code, 2, "{}", out);
    assert!(
        out.contains("Core's sources changed since it was last built"),
        "{}",
        out
    );
    fs::write(&core, "class Object;\n").unwrap();
    assert_eq!(build(root, &[]).0, 0);

    let (code, out) = build(root, &["--clean"]);
    assert_eq!(code, 2, "{}", out);
    assert!(
        out.contains("Core would be rebuilt by a clean build"),
        "{}",
        out
    );

    fs::remove_file(root.join("System/Core.u")).unwrap();
    let (code, out) = build(root, &[]);
    assert_eq!(code, 2, "{}", out);
    assert!(
        out.contains("Core isn't compiled, so UCC would compile it"),
        "{}",
        out
    );
    assert_eq!(made(root), "");
}

#[cfg(unix)]
#[test]
fn locked_packages_stop_a_cascade() {
    let dir = modless_game("locked = [\"Core\", \"C\"]\n");
    let root = dir.path();
    // C has no record yet, but it's locked, so it's taken as it is: that
    // needs a compiled C.
    fs::write(root.join("System/C.u"), "old").unwrap();
    let (code, out) = build(root, &[]);
    assert_eq!(code, 2, "{}", out);
    assert!(
        out.contains("C comes after a package that's being rebuilt"),
        "{}",
        out
    );
    let (code, out) = build(root, &["--ignore-dependencies"]);
    assert_eq!(code, 0, "{}", out);
    assert_eq!(made(root), "A\nB\n");
    assert_eq!(fs::read_to_string(root.join("System/C.u")).unwrap(), "old");
}

#[cfg(unix)]
#[test]
fn modless_clean_without_locks_rebuilds_everything() {
    let dir = modless_game("");
    let root = dir.path();
    let (code, out) = build(root, &["--clean"]);
    assert_eq!(code, 0, "{}", out);
    // Every package with sources; NoSource can't be rebuilt, so it's kept.
    assert_eq!(made(root), "Core\nA\nB\nC\n");
    assert!(
        out.contains("Exported localization: 4 succeeded"),
        "{}",
        out
    );
    assert_eq!(
        fs::read_to_string(root.join("System/NoSource.u")).unwrap(),
        "stock"
    );

    fs::remove_file(root.join("System/NoSource.u")).unwrap();
    let (code, out) = build(root, &[]);
    assert_eq!(code, 2, "{}", out);
    assert!(out.contains("NoSource has no sources"), "{}", out);
}

#[cfg(unix)]
#[test]
fn modless_build_chooses_the_ini() {
    let dir = modless_game("");
    let root = dir.path();
    fs::write(
        root.join("System/Other.ini"),
        "[Editor.EditorEngine]\nEditPackages=A\n",
    )
    .unwrap();
    let (code, out) = build(root, &[]);
    assert_eq!(code, 2, "{}", out);
    assert!(out.contains("more than one INI"), "{}", out);
    assert!(out.contains("Game.ini, Other.ini"), "{}", out);

    fs::write(
        root.join("usagi.toml"),
        "[build]\nwine = \"sh\"\nini = \"other.ini\"\n",
    )
    .unwrap();
    let (code, out) = build(root, &[]);
    assert_eq!(code, 0, "{}", out);
    assert_eq!(made(root), "A\n");

    fs::write(root.join("usagi.toml"), "[build]\nini = \"Missing.ini\"\n").unwrap();
    let (code, out) = build(root, &[]);
    assert_eq!(code, 2, "{}", out);
    assert!(
        out.contains("build.ini \"Missing.ini\" not found"),
        "{}",
        out
    );
}

#[cfg(unix)]
#[test]
fn no_mod_overrides_build_mod() {
    let dir = modless_game("");
    let root = dir.path();
    fs::write(
        root.join("usagi.toml"),
        "[build]\nmod = \"Missing\"\nwine = \"sh\"\nversion = \"1.0.0\"\nversion-class = \"A/Classes/V.uc\"\n",
    )
    .unwrap();
    // With a mod, the version belongs in the mod's file.
    let (code, out) = build(root, &[]);
    assert_eq!(code, 2, "{}", out);
    assert!(
        out.contains("when building a mod, build.version"),
        "{}",
        out
    );
    // Without one, it's the game directory's.
    let (code, out) = build(root, &["--no-mod"]);
    assert_eq!(code, 0, "{}", out);
    assert!(root.join("A/Classes/V.uc").is_file());
    usagi(root)
        .args(["build", "--no-mod", "--mod", "X"])
        .assert()
        .code(2);
}

#[cfg(unix)]
#[test]
fn a_mod_package_left_in_system_is_always_removed() {
    let dir = game();
    let root = dir.path();
    fs::write(root.join("System/A.u"), "left over").unwrap();
    fs::write(
        root.join("usagi.toml"),
        "[build]\nmod = \"Mod\"\nwine = \"sh\"\nlocked = [\"*\"]\n",
    )
    .unwrap();
    let (code, out) = build(root, &[]);
    assert_eq!(code, 0, "{}", out);
    assert!(out.contains("Removing A.u left in"), "{}", out);
    assert!(!root.join("System/A.u").exists());
    assert!(root.join("Mod/System/A.u").is_file());
}

#[cfg(unix)]
#[test]
fn locking_everything_leaves_a_mods_own_packages_buildable() {
    let dir = game();
    let root = dir.path();
    fs::write(
        root.join("usagi.toml"),
        "[build]\nmod = \"Mod\"\nwine = \"sh\"\nlocked = [\"*\"]\n",
    )
    .unwrap();
    let (code, out) = build(root, &[]);
    assert_eq!(code, 0, "{}", out);
    assert_eq!(made(root), "A\nB\nC\n");
    fs::write(
        root.join("B/Classes/B.uc"),
        "class B extends A; // changed\n",
    )
    .unwrap();
    let (code, out) = build(root, &[]);
    assert_eq!(code, 0, "{}", out);
    assert_eq!(made(root), "B\nC\n");
    // Without the mod, the same packages are locked.
    fs::create_dir_all(root.join("Core/Classes")).unwrap();
    fs::write(root.join("Core/Classes/Object.uc"), "class Object;\n").unwrap();
    fs::write(
        root.join("System/Game.ini"),
        "[Editor.EditorEngine]\nEditPackages=Core\n",
    )
    .unwrap();
    let (code, out) = build(root, &["--no-mod", "--clean"]);
    assert_eq!(code, 2, "{}", out);
    assert!(
        out.contains("Core would be rebuilt by a clean build"),
        "{}",
        out
    );
}

#[test]
fn class_name_mismatch() {
    let dir = project(&[("Foo.uc", CLEAN), ("Bar.uc", CLEAN)]);
    let out = concise(dir.path(), &[]);
    assert!(out.contains("Foo.uc:") && out.contains("C016"), "{}", out);
    assert!(!out.contains("Bar.uc:"), "{}", out);
    let out = concise(dir.path(), &["--ignore", "C016"]);
    assert!(!out.contains("C016"), "{}", out);
}

/// A package with a UT2004 summary, two generations and no tables.
fn package_bytes() -> Vec<u8> {
    let mut out = Vec::new();
    for value in [0x9E2A83C1u32, 128 | 29 << 16, 0x10021, 0, 0, 0, 0, 0, 0] {
        out.extend_from_slice(&value.to_le_bytes());
    }
    for value in [
        0x01234567u32,
        0x89ABCDEF,
        0xDEADBEEF,
        0x2A,
        2,
        5,
        40,
        1234,
        4567,
    ] {
        out.extend_from_slice(&value.to_le_bytes());
    }
    out
}

#[test]
fn pkg_list_prints_the_summary() {
    let dir = TempDir::new().unwrap();
    fs::write(dir.path().join("Test.u"), package_bytes()).unwrap();
    usagi(dir.path())
        .args(["pkg", "list", "Test.u"])
        .assert()
        .code(0)
        .stdout(
            "Test.u
  Tag          0x9E2A83C1
  Version      128 (licensee 29)
  Flags        0x00010021 (AllowDownload, Official, unknown 0x00010000)
  GUID         0123456789ABCDEFDEADBEEF0000002A
  Names        0 at 0x0
  Imports      0 at 0x0
  Exports      0 at 0x0
  Size         72 bytes
  Generations  2
    1     5 exports    40 names
    2  1234 exports  4567 names

Dependencies (0)
",
        );
}

#[test]
fn pkg_list_json() {
    let dir = TempDir::new().unwrap();
    fs::write(dir.path().join("Test.u"), package_bytes()).unwrap();
    let output = usagi(dir.path())
        .args(["pkg", "list", "Test.u", "--format", "json"])
        .output()
        .unwrap();
    assert_eq!(output.status.code(), Some(0));
    let json: serde_json::Value = serde_json::from_slice(&output.stdout).unwrap();
    assert_eq!(json["path"], "Test.u");
    assert_eq!(json["size"], 72);
    assert_eq!(json["compressed_size"], serde_json::Value::Null);
    assert_eq!(json["version"], 128);
    assert_eq!(json["licensee_version"], 29);
    let summary = &json["summary"];
    assert_eq!(summary["tag"], 0x9E2A83C1u32);
    assert_eq!(summary["package_flags"]["bits"], 0x10021);
    assert_eq!(
        summary["package_flags"]["names"],
        serde_json::json!(["AllowDownload", "Official"])
    );
    assert_eq!(summary["guid"], "0123456789ABCDEFDEADBEEF0000002A");
    assert_eq!(
        summary["generations"][1],
        serde_json::json!({ "export_count": 1234, "name_count": 4567 })
    );
    assert_eq!(summary["heritage"], serde_json::Value::Null);
}

#[test]
fn pkg_list_old_and_compressed_packages() {
    use std::io::Write;
    let dir = TempDir::new().unwrap();
    // Version 60, no flags, an empty heritage table at offset 99.
    let mut old = Vec::new();
    for value in [0x9E2A83C1u32, 60, 0, 0, 0, 0, 0, 0, 0, 0, 99] {
        old.extend_from_slice(&value.to_le_bytes());
    }
    fs::write(dir.path().join("Old.u"), &old).unwrap();
    usagi(dir.path())
        .args(["pkg", "list", "Old.u"])
        .assert()
        .code(0)
        .stdout(predicate::str::contains(
            "  Version      60 (licensee 0)\n  Flags        0x00000000\n",
        ))
        .stdout(predicate::str::contains("  Heritage     0 GUIDs at 0x63\n"))
        .stdout(predicate::str::contains(
            "  Generations  1\n    1  0 exports  0 names\n",
        ));

    let bytes = package_bytes();
    let mut encoder = flate2::write::ZlibEncoder::new(Vec::new(), flate2::Compression::default());
    encoder.write_all(&bytes).unwrap();
    let data = encoder.finish().unwrap();
    let mut uz2 = Vec::new();
    uz2.extend_from_slice(&(data.len() as u32).to_le_bytes());
    uz2.extend_from_slice(&(bytes.len() as u32).to_le_bytes());
    uz2.extend_from_slice(&data);
    fs::write(dir.path().join("Test.u.uz2"), &uz2).unwrap();
    usagi(dir.path())
        .args(["pkg", "list", "Test.u.uz2"])
        .assert()
        .code(0)
        .stdout(predicate::str::contains(format!(
            "  Size         72 bytes ({} compressed)\n",
            uz2.len()
        )));
}

#[test]
fn pkg_list_red_orchestra_packages() {
    let dir = TempDir::new().unwrap();
    let mut bytes = package_bytes();
    bytes[0] = 0xC2;
    fs::write(dir.path().join("Test.ukx"), bytes).unwrap();
    usagi(dir.path())
        .args(["pkg", "list", "Test.ukx"])
        .assert()
        .code(0)
        .stdout(predicate::str::contains(
            "Test.ukx\n  Tag          0x9E2A83C2 (Red Orchestra, Killing Floor)\n  Version      128 (licensee 29)\n",
        ));
}

#[test]
fn pkg_list_errors() {
    let dir = project(&[("Foo.uc", CLEAN)]);
    usagi(dir.path())
        .args(["pkg", "list", "Missing.u"])
        .assert()
        .code(2)
        .stderr(predicate::str::starts_with("usagi: Missing.u: "));
    usagi(dir.path())
        .args(["pkg", "list", "Foo.uc"])
        .assert()
        .code(2)
        .stderr(
            "usagi: Foo.uc: not an Unreal package (the tag is 0x73616C63, not 0x9E2A83C1 or 0x9E2A83C2)\n",
        );
    usagi(dir.path()).args(["pkg", "list"]).assert().code(2);
}

/// A compact index, as the engine writes it.
fn compact_index(value: i32) -> Vec<u8> {
    let mut v = value.unsigned_abs();
    let mut out = vec![(if value < 0 { 0x80 } else { 0 }) | (v & 0x3f) as u8];
    if v >= 0x40 {
        out[0] |= 0x40;
        v >>= 6;
        loop {
            let more = v >= 0x80;
            out.push((v & 0x7f) as u8 | if more { 0x80 } else { 0 });
            if !more {
                break;
            }
            v >>= 7;
        }
    }
    out
}

/// `MyPkg`: a package with these imports and exports.
///
/// ```text
/// -1  Core.Package  Core
/// -2  Core.Package  Engine
/// -3  Core.Class    Engine.Texture
/// -4  Core.Class    Core.Package
/// -5  Core.Class      Engine.Actor
/// -6  Core.Package    Engine.Group      (a group)
/// -7  Engine.Texture  Engine.Group.Rock
///  1  Class         MyClass        (extends Engine.Actor)
///  2  Package       MyGroup
///  3  Texture       MyGroup.MyTex
///  4  Class         None           (no data)
/// ```
fn tables_package_bytes() -> Vec<u8> {
    let names = [
        "None", "Core", "Package", "Class", "Engine", "Texture", "MyGroup", "MyTex", "MyClass",
        "Actor", "Group", "Rock",
    ];
    // (class package, class name, outer, object name)
    let imports = [
        (1, 2, 0, 1),
        (1, 2, 0, 4),
        (1, 3, -2, 5),
        (1, 3, -1, 2),
        (1, 3, -2, 9),
        (1, 2, -2, 10),
        (4, 5, -6, 11),
    ];
    // (class, super, outer, object name, flags, size)
    let exports = [
        (0, -5, 0, 8, 0x040F0004u32, 40),
        (-4, 0, 0, 6, 0x00070004, 1),
        (-3, 0, 2, 7, 0x00070004, 5),
        (0, 0, 0, 0, 0, 0),
    ];
    let mut body = Vec::new();
    for name in names {
        body.extend(compact_index(name.len() as i32 + 1));
        body.extend(name.as_bytes());
        body.push(0);
        body.extend(0x00070010u32.to_le_bytes());
    }
    let mut offsets = Vec::new();
    for export in &exports {
        offsets.push(64 + body.len() as i32);
        body.resize(body.len() + export.5 as usize, 0);
    }
    let import_offset = 64 + body.len() as i32;
    for (class_package, class_name, outer, object_name) in imports {
        body.extend(compact_index(class_package));
        body.extend(compact_index(class_name));
        body.extend((outer as i32).to_le_bytes());
        body.extend(compact_index(object_name));
    }
    let export_offset = 64 + body.len() as i32;
    for (export, offset) in exports.iter().zip(offsets) {
        let (class, super_struct, outer, object_name, flags, size) = *export;
        body.extend(compact_index(class));
        body.extend(compact_index(super_struct));
        body.extend((outer as i32).to_le_bytes());
        body.extend(compact_index(object_name));
        body.extend(flags.to_le_bytes());
        body.extend(compact_index(size));
        if size != 0 {
            body.extend(compact_index(offset));
        }
    }
    let mut out = Vec::new();
    let header: [i32; 9] = [
        0x9E2A83C1u32 as i32,
        128 | 29 << 16,
        0x21,
        names.len() as i32,
        64,
        exports.len() as i32,
        export_offset,
        imports.len() as i32,
        import_offset,
    ];
    for value in header {
        out.extend(value.to_le_bytes());
    }
    for value in [0x01234567u32, 0x89ABCDEF, 0xDEADBEEF, 0x2A, 1, 4, 10] {
        out.extend(value.to_le_bytes());
    }
    out.extend(body);
    out
}

fn tables_dir() -> TempDir {
    let dir = TempDir::new().unwrap();
    fs::write(dir.path().join("MyPkg.u"), tables_package_bytes()).unwrap();
    dir
}

const DEPENDENCIES_TEXT: &str = "
Dependencies (2)
  Package  Imports  Classes
  Core           1  Class (1)
  Engine         3  Class (2), Texture (1)
";

const GROUPS_TEXT: &str = "
Groups (1)
  Group          Objects  Classes
  MyPkg.MyGroup        1  Texture (1)
";

const IMPORTS_TEXT: &str = "
Imports (7)
   #  Class           Path
  -1  Core.Package    Core
  -2  Core.Package    Engine
  -3  Core.Class      Engine.Texture
  -4  Core.Class      Core.Package
  -5  Core.Class      Engine.Actor
  -6  Core.Package    Engine.Group
  -7  Engine.Texture  Engine.Group.Rock
";

const EXPORTS_TEXT: &str = "
Exports (4)
  #  Class    Path                 Size  Offset  Super         Flags
  1  Class    MyPkg.MyClass          40    0xCA  Engine.Actor  Public,LoadForClient,LoadForServer,LoadForEdit,Standalone,Native
  2  Package  MyPkg.MyGroup           1    0xF2                Public,LoadForClient,LoadForServer,LoadForEdit
  3  Texture  MyPkg.MyGroup.MyTex     5    0xF3                Public,LoadForClient,LoadForServer,LoadForEdit
  4  Class    MyPkg.None              0
";

fn pkg_text(dir: &Path, args: &[&str]) -> String {
    let output = usagi(dir)
        .args(["pkg", "list", "MyPkg.u", "--color", "never"])
        .args(args)
        .output()
        .unwrap();
    assert_eq!(output.status.code(), Some(0), "{:?}", output);
    String::from_utf8(output.stdout).unwrap()
}

#[test]
fn pkg_list_imports_and_exports() {
    let dir = tables_dir();
    let all = format!(
        "MyPkg.u\n{}{}{}{}",
        DEPENDENCIES_TEXT, GROUPS_TEXT, IMPORTS_TEXT, EXPORTS_TEXT
    );
    assert_eq!(pkg_text(dir.path(), &["--all", "--no-summary"]), all);
    assert_eq!(
        pkg_text(
            dir.path(),
            &[
                "--dependencies",
                "--groups",
                "--imports",
                "--exports",
                "--no-summary"
            ]
        ),
        all
    );
    assert_eq!(
        pkg_text(dir.path(), &["--imports", "--exports", "--no-summary"]),
        format!("MyPkg.u\n{}{}", IMPORTS_TEXT, EXPORTS_TEXT)
    );
    assert_eq!(
        pkg_text(dir.path(), &["--imports", "--no-summary"]),
        format!("MyPkg.u\n{}", IMPORTS_TEXT)
    );
    assert_eq!(
        pkg_text(dir.path(), &["--exports", "--no-summary"]),
        format!("MyPkg.u\n{}", EXPORTS_TEXT)
    );
    // With no table asked for, the dependencies are listed.
    let dependencies = format!("MyPkg.u\n{}", DEPENDENCIES_TEXT);
    assert_eq!(pkg_text(dir.path(), &["--no-summary"]), dependencies);
    assert_eq!(
        pkg_text(dir.path(), &["--dependencies", "--no-summary"]),
        dependencies
    );
    // With the summary, the tables follow it.
    let text = pkg_text(dir.path(), &[]);
    assert!(
        text.starts_with("MyPkg.u\n  Tag          0x9E2A83C1\n"),
        "{}",
        text
    );
    assert!(text.ends_with(DEPENDENCIES_TEXT), "{}", text);
    let text = pkg_text(dir.path(), &["--exports"]);
    assert!(text.ends_with(EXPORTS_TEXT), "{}", text);
    assert!(!text.contains("Dependencies ("), "{}", text);
}

#[test]
fn pkg_list_concise() {
    let dir = tables_dir();
    let concise = |args: &[&str]| pkg_text(dir.path(), &[&["--format", "concise"], args].concat());
    // The dependencies' names, without the summary.
    assert_eq!(concise(&[]), "Core\nEngine\n");
    assert_eq!(concise(&["--sort", "size"]), "Engine\nCore\n");
    assert_eq!(
        concise(&["--imports"]),
        "Core\nEngine\nEngine.Texture\nCore.Package\nEngine.Actor\nEngine.Group\nEngine.Group.Rock\n"
    );
    assert_eq!(
        concise(&["--exports", "--sort", "path", "--reverse"]),
        "MyPkg.None\nMyPkg.MyGroup.MyTex\nMyPkg.MyGroup\nMyPkg.MyClass\n"
    );
    let all = concise(&["--all"]);
    assert!(
        all.starts_with("Core\nEngine\nMyPkg.MyGroup\nCore\n"),
        "{}",
        all
    );
    assert!(
        all.ends_with(
            "Engine.Group.Rock\nMyPkg.MyClass\nMyPkg.MyGroup\nMyPkg.MyGroup.MyTex\nMyPkg.None\n"
        ),
        "{}",
        all
    );
}

#[test]
fn pkg_list_tables_json() {
    let dir = tables_dir();
    let json = |args: &[&str]| -> serde_json::Value {
        let output = usagi(dir.path())
            .args([
                "pkg", "list", "MyPkg.u", "--format", "json", "--color", "always",
            ])
            .args(args)
            .output()
            .unwrap();
        assert_eq!(output.status.code(), Some(0));
        assert!(!output.stdout.contains(&0x1b), "JSON is never coloured");
        serde_json::from_slice(&output.stdout).unwrap()
    };
    let all = json(&["--all"]);
    assert_eq!(all["summary"]["import_count"], 7);
    assert_eq!(
        all["dependencies"],
        serde_json::json!([
            { "name": "Core", "index": -1, "import_count": 1, "classes": [{ "name": "Class", "count": 1 }] },
            {
                "name": "Engine",
                "index": -2,
                "import_count": 3,
                "classes": [{ "name": "Class", "count": 2 }, { "name": "Texture", "count": 1 }],
            },
        ])
    );
    assert_eq!(
        all["imports"][2],
        serde_json::json!({
            "index": -3,
            "class_package": "Core",
            "class_name": "Class",
            "outer_index": -2,
            "object_name": "Texture",
            "path": "Engine.Texture",
        })
    );
    assert_eq!(
        all["exports"][0],
        serde_json::json!({
            "index": 1,
            "class_index": 0,
            "class_name": "Class",
            "super_index": -5,
            "super_path": "Engine.Actor",
            "outer_index": 0,
            "object_name": "MyClass",
            "path": "MyPkg.MyClass",
            "flags": {
                "bits": 0x040F0004,
                "names": ["Public", "LoadForClient", "LoadForServer", "LoadForEdit", "Standalone", "Native"],
                "unknown": 0,
            },
            "serial_size": 40,
            "serial_offset": 202,
        })
    );
    assert_eq!(all["exports"][2]["path"], "MyPkg.MyGroup.MyTex");
    assert_eq!(all["exports"][2]["super_path"], serde_json::Value::Null);
    assert_eq!(all["exports"][3]["serial_offset"], serde_json::Value::Null);

    // --no-summary leaves out the summary and its sizes; the tables are
    // only there when asked for.
    let exports = json(&["--exports", "--no-summary"]);
    let mut keys: Vec<&String> = exports.as_object().unwrap().keys().collect();
    keys.sort();
    assert_eq!(keys, ["exports", "path"]);
    let summary = json(&[]);
    assert!(summary.get("imports").is_none() && summary.get("exports").is_none());
    assert!(summary.get("summary").is_some());
}

#[test]
fn pkg_list_colour() {
    let dir = tables_dir();
    let output = usagi(dir.path())
        .args(["pkg", "list", "MyPkg.u", "--all", "--color", "always"])
        .output()
        .unwrap();
    let text = String::from_utf8(output.stdout).unwrap();
    for expected in [
        "\x1b[1mMyPkg.u\x1b[0m\n",
        "\x1b[36m  Version      \x1b[0m128",
        "(\x1b[32mAllowDownload\x1b[0m, \x1b[32mOfficial\x1b[0m)",
        "\x1b[1mImports (7)\x1b[0m",
        "  \x1b[2m-3\x1b[0m  \x1b[36mCore.Class\x1b[0m      Engine.Texture\n",
        "  Engine         3  \x1b[36mClass (2), Texture (1)\x1b[0m\n",
        "  \x1b[1m#\x1b[0m  \x1b[1mClass\x1b[0m    \x1b[1mPath\x1b[0m",
        "\x1b[32mPublic,LoadForClient,LoadForServer,LoadForEdit\x1b[0m",
    ] {
        assert!(text.contains(expected), "{:?} not in {:?}", expected, text);
    }
    // Unknown package flags are red, and a Tripwire tag's note is dim.
    let mut bytes = package_bytes();
    bytes[0] = 0xC2;
    fs::write(dir.path().join("Ro.u"), bytes).unwrap();
    let output = usagi(dir.path())
        .args(["pkg", "list", "Ro.u", "--color", "always"])
        .output()
        .unwrap();
    let text = String::from_utf8(output.stdout).unwrap();
    assert!(
        text.contains("\x1b[31munknown 0x00010000\x1b[0m"),
        "{:?}",
        text
    );
    assert!(
        text.contains("\x1b[2m(Red Orchestra, Killing Floor)\x1b[0m"),
        "{:?}",
        text
    );
    assert!(!pkg_text(dir.path(), &["--all"]).contains('\x1b'));
}

#[cfg(unix)]
#[test]
fn a_closed_pipe_stops_quietly() {
    use std::io::Read;
    use std::os::unix::process::ExitStatusExt;
    use std::process::Stdio;
    // Close the read end before `usagi` writes, as `head` does once it has
    // what it needs. `usagi` is stopped by SIGPIPE, without an error.
    let mut child = std::process::Command::new(assert_cmd::cargo::cargo_bin("usagi"))
        .args(["rule", "--all"])
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    drop(child.stdout.take());
    let mut stderr = String::new();
    child
        .stderr
        .take()
        .unwrap()
        .read_to_string(&mut stderr)
        .unwrap();
    let status = child.wait().unwrap();
    assert_eq!(stderr, "");
    assert!(
        status.success() || status.signal() == Some(libc::SIGPIPE),
        "{:?}",
        status
    );
}

/// The export or import numbers in a table of `pkg list` output, in order.
fn table_numbers(text: &str, table: &str) -> Vec<i32> {
    text.split(&format!("{} (", table))
        .nth(1)
        .unwrap()
        .lines()
        .skip(2)
        .take_while(|line| !line.is_empty())
        .map(|line| line.split_whitespace().next().unwrap().parse().unwrap())
        .collect()
}

#[test]
fn pkg_list_sorts_the_tables() {
    let dir = tables_dir();
    let order = |args: &[&str]| {
        let text = pkg_text(dir.path(), &[&["--all", "--no-summary"], args].concat());
        (
            table_numbers(&text, "Imports"),
            table_numbers(&text, "Exports"),
        )
    };
    let all = vec![-1, -2, -3, -4, -5, -6, -7];
    assert_eq!(order(&[]), (all.clone(), vec![1, 2, 3, 4]));
    assert_eq!(
        order(&["--sort", "index", "--reverse"]),
        (vec![-7, -6, -5, -4, -3, -2, -1], vec![4, 3, 2, 1])
    );
    // Paths: Core, Core.Package, Engine, Engine.Actor, Engine.Group,
    // Engine.Group.Rock, Engine.Texture; and MyPkg.MyClass, MyPkg.MyGroup,
    // MyPkg.MyGroup.MyTex, MyPkg.None.
    assert_eq!(
        order(&["--sort", "path"]),
        (vec![-1, -4, -2, -5, -6, -7, -3], vec![1, 2, 3, 4])
    );
    // Classes: Class, Package and Texture. Ties keep table order, even
    // reversed.
    assert_eq!(
        order(&["--sort", "class"]),
        (vec![-3, -4, -5, -1, -2, -6, -7], vec![1, 4, 2, 3])
    );
    assert_eq!(
        order(&["--sort", "class", "--reverse"]),
        (vec![-7, -1, -2, -6, -3, -4, -5], vec![3, 2, 1, 4])
    );
    // Size: largest first; imports have no size, so keep table order.
    assert_eq!(order(&["--sort", "size"]), (all, vec![1, 3, 2, 4]));
    assert_eq!(order(&["--sort", "size", "--reverse"]).1, vec![4, 2, 3, 1]);
    // Offset: exports with no data last, so first when reversed. (The
    // data is in table order, as the engine writes it.)
    assert_eq!(order(&["--sort", "offset"]).1, vec![1, 2, 3, 4]);
    assert_eq!(
        order(&["--sort", "offset", "--reverse"]).1,
        vec![4, 3, 2, 1]
    );

    // JSON is sorted the same way.
    let output = usagi(dir.path())
        .args([
            "pkg",
            "list",
            "MyPkg.u",
            "--exports",
            "--format",
            "json",
            "--sort",
            "size",
        ])
        .output()
        .unwrap();
    let json: serde_json::Value = serde_json::from_slice(&output.stdout).unwrap();
    let indices: Vec<i64> = json["exports"]
        .as_array()
        .unwrap()
        .iter()
        .map(|e| e["index"].as_i64().unwrap())
        .collect();
    assert_eq!(indices, [1, 3, 2, 4]);
}

#[test]
fn pkg_list_humanize() {
    let dir = tables_dir();
    let text = pkg_text(dir.path(), &["--all", "--humanize"]);
    for expected in [
        "  Size         351 B\n",
        "  Exports      4 at 0x129\n",
        "  1  Class    MyPkg.MyClass        40 B    0xCA  Engine.Actor",
        "  4  Class    MyPkg.None            0 B\n",
    ] {
        assert!(text.contains(expected), "{:?} not in {}", expected, text);
    }
    // Units and separators, on the summary of a package whose sizes and
    // counts are large: 15,530 names (zero bytes are empty names) in a
    // 2.6 MB file.
    let mut bytes = package_bytes();
    bytes[12..16].copy_from_slice(&15530i32.to_le_bytes());
    bytes[16..20].copy_from_slice(&100i32.to_le_bytes());
    bytes.resize(2_722_417, 0);
    fs::write(dir.path().join("Big.u"), &bytes).unwrap();
    let output = usagi(dir.path())
        .args(["pkg", "list", "Big.u", "--humanize", "--color", "never"])
        .output()
        .unwrap();
    let text = String::from_utf8(output.stdout).unwrap();
    assert!(text.contains("  Names        15,530 at 0x64\n"), "{}", text);
    assert!(text.contains("  Size         2.6 MB\n"), "{}", text);
    assert!(
        text.contains("    2  1,234 exports  4,567 names\n"),
        "{}",
        text
    );
    // Without --humanize, the raw numbers.
    let output = usagi(dir.path())
        .args(["pkg", "list", "Big.u", "--color", "never"])
        .output()
        .unwrap();
    let text = String::from_utf8(output.stdout).unwrap();
    assert!(text.contains("  Names        15530 at 0x64\n"), "{}", text);
    assert!(text.contains("  Size         2722417 bytes\n"), "{}", text);
}

const OBJECTS_TEXT: &str = "
Dependencies (2)
  Package     Imports  Classes
  Core              1  Class (1)
  └─ Package           Class
  Engine            3  Class (2), Texture (1)
  ├─ Actor             Class
  ├─ Group
  │  └─ Rock           Texture
  └─ Texture           Class
";

#[test]
fn pkg_list_dependency_objects() {
    let dir = tables_dir();
    let objects = format!("MyPkg.u\n{}", OBJECTS_TEXT);
    assert_eq!(
        pkg_text(dir.path(), &["--dependencies=objects", "--no-summary"]),
        objects
    );
    // --all lists the packages only, unless asked for the objects.
    let all = pkg_text(dir.path(), &["--all", "--no-summary"]);
    assert!(all.starts_with(&format!("MyPkg.u\n{}", DEPENDENCIES_TEXT)));
    let all = pkg_text(
        dir.path(),
        &["--all", "--dependencies=objects", "--no-summary"],
    );
    assert!(all.starts_with(&objects), "{}", all);
    assert_eq!(
        pkg_text(dir.path(), &["--dependencies=packages", "--no-summary"]),
        format!("MyPkg.u\n{}", DEPENDENCIES_TEXT)
    );
    // The level needs an `=`, so a path after --dependencies isn't one.
    usagi(dir.path())
        .args(["pkg", "list", "--dependencies", "MyPkg.u", "--no-summary"])
        .assert()
        .code(0);
    usagi(dir.path())
        .args(["pkg", "list", "MyPkg.u", "--dependencies=everything"])
        .assert()
        .code(2);

    // Concise: each package, then its objects' full paths.
    assert_eq!(
        pkg_text(
            dir.path(),
            &["--dependencies=objects", "--format", "concise"]
        ),
        "Core\nCore.Package\nEngine\nEngine.Actor\nEngine.Group.Rock\nEngine.Texture\n"
    );

    // Colour: classes cyan, in the Classes column.
    let output = usagi(dir.path())
        .args([
            "pkg",
            "list",
            "MyPkg.u",
            "--dependencies=objects",
            "--color",
            "always",
        ])
        .output()
        .unwrap();
    let text = String::from_utf8(output.stdout).unwrap();
    assert!(
        text.contains("  ├─ Actor             \x1b[36mClass\x1b[0m\n"),
        "{:?}",
        text
    );
    assert!(text.contains("  ├─ Group\n"), "{:?}", text);

    // JSON: each dependency's objects, sorted by path.
    let output = usagi(dir.path())
        .args([
            "pkg",
            "list",
            "MyPkg.u",
            "--dependencies=objects",
            "--format",
            "json",
        ])
        .output()
        .unwrap();
    let json: serde_json::Value = serde_json::from_slice(&output.stdout).unwrap();
    assert_eq!(
        json["dependencies"][1]["objects"],
        serde_json::json!([
            { "index": -5, "path": "Engine.Actor", "class_package": "Core", "class_name": "Class" },
            { "index": -7, "path": "Engine.Group.Rock", "class_package": "Engine", "class_name": "Texture" },
            { "index": -3, "path": "Engine.Texture", "class_package": "Core", "class_name": "Class" },
        ])
    );
    let output = usagi(dir.path())
        .args(["pkg", "list", "MyPkg.u", "--format", "json"])
        .output()
        .unwrap();
    let json: serde_json::Value = serde_json::from_slice(&output.stdout).unwrap();
    assert!(json["dependencies"][1].get("objects").is_none());
}

#[test]
fn pkg_list_groups() {
    let dir = tables_dir();
    assert_eq!(
        pkg_text(dir.path(), &["--groups", "--no-summary"]),
        format!("MyPkg.u\n{}", GROUPS_TEXT)
    );
    assert_eq!(
        pkg_text(dir.path(), &["--groups", "--format", "concise"]),
        "MyPkg.MyGroup\n"
    );
    let output = usagi(dir.path())
        .args(["pkg", "list", "MyPkg.u", "--groups", "--format", "json"])
        .output()
        .unwrap();
    let json: serde_json::Value = serde_json::from_slice(&output.stdout).unwrap();
    assert_eq!(
        json["groups"],
        serde_json::json!([{
            "index": 2,
            "path": "MyPkg.MyGroup",
            "object_count": 1,
            "classes": [{ "name": "Texture", "count": 1 }],
        }])
    );
    // A package without groups has an empty table.
    fs::write(dir.path().join("Empty.u"), package_bytes()).unwrap();
    let output = usagi(dir.path())
        .args([
            "pkg",
            "list",
            "Empty.u",
            "--groups",
            "--no-summary",
            "--color",
            "never",
        ])
        .output()
        .unwrap();
    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        "Empty.u\n\nGroups (0)\n"
    );
}
