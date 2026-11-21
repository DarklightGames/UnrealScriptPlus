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
        .args(["lint", "rule", "self-assignment"])
        .assert()
        .code(0)
        .stdout(predicate::str::starts_with("S003 self-assignment"));
    usagi(dir.path())
        .args(["lint", "rule", "--all"])
        .assert()
        .code(0)
        .stdout(predicate::str::contains("R013"));
    usagi(dir.path())
        .args(["lint", "rule", "Z999"])
        .assert()
        .code(2);
    // `rule` is no longer a command of its own.
    usagi(dir.path()).args(["rule", "S003"]).assert().code(2);
    // Lint flags don't go with `rule`.
    usagi(dir.path())
        .args(["lint", "--fix", "rule", "S003"])
        .assert()
        .code(2);
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
        .args(["lint", "rule", "--all"])
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

/// A package with these flags and nothing else.
fn flagged_package(flags: u32) -> Vec<u8> {
    let mut bytes = package_bytes();
    bytes[8..12].copy_from_slice(&flags.to_le_bytes());
    bytes
}

fn flags_of(path: &Path) -> u32 {
    let bytes = fs::read(path).unwrap();
    u32::from_le_bytes(bytes[8..12].try_into().unwrap())
}

fn pkg_flags(dir: &Path, args: &[&str]) -> assert_cmd::assert::Assert {
    usagi(dir)
        .args(["pkg", "flags", "--color", "never"])
        .args(args)
        .assert()
}

#[test]
fn pkg_flags_shows_and_changes_flags() {
    let dir = TempDir::new().unwrap();
    let path = dir.path().join("Mod.u");
    fs::write(&path, flagged_package(0x5)).unwrap();
    pkg_flags(dir.path(), &["Mod.u"])
        .code(0)
        .stdout("Mod.u: 0x00000005 (AllowDownload, ServerSideOnly)\n");

    // A dry run changes nothing.
    pkg_flags(dir.path(), &["Mod.u", "--clear", "serversideonly", "--dry-run"])
        .code(0)
        .stdout("Mod.u: 0x00000005 (AllowDownload, ServerSideOnly) -> 0x00000001 (AllowDownload) (dry run)\n");
    assert_eq!(flags_of(&path), 0x5);

    let before = fs::read(&path).unwrap();
    pkg_flags(dir.path(), &["Mod.u", "--clear", "ServerSideOnly"])
        .code(0)
        .stdout(
            "Mod.u: 0x00000005 (AllowDownload, ServerSideOnly) -> 0x00000001 (AllowDownload)\n",
        );
    assert_eq!(flags_of(&path), 0x1);
    // Only the flags changed.
    let after = fs::read(&path).unwrap();
    assert_eq!(after[12..], before[12..]);

    // Several at once, by name or comma-separated.
    pkg_flags(
        dir.path(),
        &[
            "Mod.u",
            "--set",
            "ClientOptional,Need",
            "--set",
            "unsecure",
            "--clear",
            "AllowDownload",
        ],
    )
    .code(0)
    .stdout("Mod.u: 0x00000001 (AllowDownload) -> 0x00008012 (ClientOptional, Unsecure, Need)\n");
    assert_eq!(flags_of(&path), 0x8012);

    pkg_flags(dir.path(), &["Mod.u", "--set", "Need"])
        .code(0)
        .stdout("Mod.u: 0x00008012 (ClientOptional, Unsecure, Need) (unchanged)\n");

    // No flags.
    fs::write(&path, flagged_package(0)).unwrap();
    pkg_flags(dir.path(), &["Mod.u"])
        .code(0)
        .stdout("Mod.u: 0x00000000\n");
}

#[test]
fn pkg_flags_official_packages_need_force() {
    let dir = TempDir::new().unwrap();
    let path = dir.path().join("Core.u");
    fs::write(&path, flagged_package(0x20)).unwrap();
    pkg_flags(dir.path(), &["Core.u", "--set", "ServerSideOnly"])
        .code(2)
        .stdout("")
        .stderr(
            predicate::str::contains("Core.u: it's an Official package")
                .and(predicate::str::contains("--force")),
        );
    assert_eq!(flags_of(&path), 0x20);
    // Showing, or a change that changes nothing, is fine.
    pkg_flags(dir.path(), &["Core.u"]).code(0);
    pkg_flags(dir.path(), &["Core.u", "--set", "Official"]).code(0);
    pkg_flags(
        dir.path(),
        &["Core.u", "--set", "ServerSideOnly", "--force"],
    )
    .code(0)
    .stdout("Core.u: 0x00000020 (Official) -> 0x00000024 (ServerSideOnly, Official)\n");
    assert_eq!(flags_of(&path), 0x24);
}

/// A `.uz2` file of `bytes`, in one block.
fn uz2_file(bytes: &[u8]) -> Vec<u8> {
    use std::io::Write;
    let mut encoder = flate2::write::ZlibEncoder::new(Vec::new(), flate2::Compression::default());
    encoder.write_all(bytes).unwrap();
    let data = encoder.finish().unwrap();
    let mut uz2 = Vec::new();
    uz2.extend((data.len() as u32).to_le_bytes());
    uz2.extend((bytes.len() as u32).to_le_bytes());
    uz2.extend(data);
    uz2
}

#[test]
fn pkg_flags_several_files_and_uz2() {
    let dir = TempDir::new().unwrap();
    fs::write(dir.path().join("A.u"), flagged_package(0x1)).unwrap();
    fs::write(dir.path().join("Official.u"), flagged_package(0x20)).unwrap();
    fs::write(dir.path().join("Text.u"), "not a package").unwrap();
    fs::write(dir.path().join("B.u.uz2"), uz2_file(&flagged_package(0x1))).unwrap();

    // Each file is changed or fails on its own.
    pkg_flags(
        dir.path(),
        &[
            "A.u",
            "Official.u",
            "Text.u",
            "B.u.uz2",
            "--set",
            "ServerSideOnly",
        ],
    )
    .code(2)
    .stdout(
        "A.u: 0x00000001 (AllowDownload) -> 0x00000005 (AllowDownload, ServerSideOnly)\n\
         B.u.uz2: 0x00000001 (AllowDownload) -> 0x00000005 (AllowDownload, ServerSideOnly)\n",
    )
    .stderr(
        predicate::str::contains("usagi: Official.u: it's an Official package")
            .and(predicate::str::contains(
                "usagi: Text.u: not an Unreal package",
            ))
            .and(predicate::str::contains("usagi: 2 of 4 packages failed")),
    );
    assert_eq!(flags_of(&dir.path().join("A.u")), 0x5);
    pkg_flags(dir.path(), &["B.u.uz2"])
        .code(0)
        .stdout("B.u.uz2: 0x00000005 (AllowDownload, ServerSideOnly)\n");
}

#[test]
fn pkg_flags_usage_errors() {
    let dir = TempDir::new().unwrap();
    fs::write(dir.path().join("A.u"), flagged_package(0x1)).unwrap();
    pkg_flags(dir.path(), &["A.u", "--set", "Bogus"])
        .code(2)
        .stderr(predicate::str::contains(
            "unknown package flag \"Bogus\"; the flags are AllowDownload, ClientOptional",
        ));
    pkg_flags(
        dir.path(),
        &["A.u", "--set", "Need,Unsecure", "--clear", "need,unsecure"],
    )
    .code(2)
    .stderr("usagi: Unsecure, Need can't be both set and cleared\n");
    pkg_flags(dir.path(), &[]).code(2);
    pkg_flags(dir.path(), &["Missing.u"])
        .code(2)
        .stderr(predicate::str::starts_with("usagi: Missing.u: "));
    assert_eq!(flags_of(&dir.path().join("A.u")), 0x1);
}

#[test]
fn pkg_flags_colour() {
    let dir = TempDir::new().unwrap();
    fs::write(dir.path().join("A.u"), flagged_package(0x10001)).unwrap();
    usagi(dir.path())
        .args(["pkg", "flags", "A.u", "--color", "always"])
        .assert()
        .code(0)
        .stdout(
            "A.u: 0x00010001 (\x1b[32mAllowDownload\x1b[0m, \x1b[31munknown 0x00010000\x1b[0m)\n",
        );
}

/// A package with these names, imports (class package, class name, outer,
/// object name) and exports (class, outer, object name, flags, data).
fn package_file(
    names: &[&str],
    imports: &[(i32, i32, i32, i32)],
    exports: &[(i32, i32, i32, u32, Vec<u8>)],
) -> Vec<u8> {
    const HEADER: i32 = 64;
    let mut body = Vec::new();
    for name in names {
        body.extend(compact_index(name.len() as i32 + 1));
        body.extend(name.as_bytes());
        body.push(0);
        body.extend(0x00070010u32.to_le_bytes());
    }
    let mut offsets = Vec::new();
    for export in exports {
        offsets.push(HEADER + body.len() as i32);
        body.extend(&export.4);
    }
    let import_offset = HEADER + body.len() as i32;
    for &(class_package, class_name, outer, object_name) in imports {
        body.extend(compact_index(class_package));
        body.extend(compact_index(class_name));
        body.extend(outer.to_le_bytes());
        body.extend(compact_index(object_name));
    }
    let export_offset = HEADER + body.len() as i32;
    for (export, offset) in exports.iter().zip(offsets) {
        let (class, outer, object_name, flags, data) = export;
        body.extend(compact_index(*class));
        body.extend(compact_index(0));
        body.extend(outer.to_le_bytes());
        body.extend(compact_index(*object_name));
        body.extend(flags.to_le_bytes());
        body.extend(compact_index(data.len() as i32));
        if !data.is_empty() {
            body.extend(compact_index(offset));
        }
    }
    let mut out = Vec::new();
    for value in [
        0x9E2A83C1u32 as i32,
        128 | 29 << 16,
        0x1,
        names.len() as i32,
        HEADER,
        exports.len() as i32,
        export_offset,
        imports.len() as i32,
        import_offset,
    ] {
        out.extend(value.to_le_bytes());
    }
    for value in [0x01234567u32, 0x89ABCDEF, 0xDEADBEEF, 0x2A, 1, 4, 10] {
        out.extend(value.to_le_bytes());
    }
    out.extend(body);
    out
}

const OBJ_NAMES: &[&str] = &[
    "None",
    "Core",
    "Package",
    "Class",
    "Engine",
    "Texture",
    "Combiner",
    "UI",
    "Button",
    "Bar",
    "Mix",
    "MyClass",
    "Broken",
    "Format",
    "USize",
    "bMasked",
    "Region",
    "Zone",
    "Pos",
    "Vector",
    "Materials",
    "Material1",
    "Plane",
    "Tag",
    "DefaultTexture",
    "Actor",
    "Odd",
    "Scale",
    "Text",
    "OnClick",
    "Rot",
    "Rotator",
    "Tint",
    "Color",
    "Items",
    "Blob",
    "Inner",
];

fn obj_name(text: &str) -> i32 {
    OBJ_NAMES.iter().position(|&n| n == text).unwrap() as i32
}

/// A property tag for a value of `size` bytes (at most 255), with no array
/// index, then the value.
fn obj_prop(name: &str, kind: u8, struct_name: Option<&str>, value: &[u8]) -> Vec<u8> {
    let mut out = compact_index(obj_name(name));
    let size = value.len();
    let bits = match size {
        1 => 0x00,
        2 => 0x10,
        4 => 0x20,
        12 => 0x30,
        16 => 0x40,
        _ => 0x50,
    };
    out.push(kind | bits);
    if let Some(struct_name) = struct_name {
        out.extend(compact_index(obj_name(struct_name)));
    }
    if bits == 0x50 {
        out.push(size as u8);
    }
    out.extend(value);
    out
}

/// ```text
/// -1  Core.Package  Core
/// -2  Core.Package  Engine
/// -3  Core.Class    Engine.Texture
/// -4  Core.Class    Engine.Combiner
/// -5  Core.Class    Core.Package
/// -6  Engine.Texture  Engine.DefaultTexture
///  1  Package   UI
///  2  Texture   UI.Button   (properties of most types, then native data)
///  3  Texture   UI.Bar
///  4  Combiner  Mix
///  5  Class     MyClass
///  6  Texture   Broken      (a property tag cut short)
///  7  Texture   Odd         (a state frame, then the other types)
/// ```
fn obj_package() -> Vec<u8> {
    let n = obj_name;
    let imports = [
        (n("Core"), n("Package"), 0, n("Core")),
        (n("Core"), n("Package"), 0, n("Engine")),
        (n("Core"), n("Class"), -2, n("Texture")),
        (n("Core"), n("Class"), -2, n("Combiner")),
        (n("Core"), n("Class"), -1, n("Package")),
        (n("Engine"), n("Texture"), -2, n("DefaultTexture")),
    ];
    let mut button = obj_prop("Format", 1, None, &[5]);
    button.extend(obj_prop("USize", 2, None, &256i32.to_le_bytes()));
    // bMasked=True: the value is the tag's array bit.
    button.extend([n("bMasked") as u8, 0x80 | 0x50 | 3, 0]);
    // Tag(2)=Actor
    button.extend([n("Tag") as u8, 0x80 | 6, 2, n("Actor") as u8]);
    let vector: Vec<u8> = [1.0f32, 2.5, -3.0]
        .iter()
        .flat_map(|v| v.to_le_bytes())
        .collect();
    button.extend(obj_prop("Pos", 10, Some("Vector"), &vector));
    let mut region = obj_prop("Zone", 5, None, &[0x86]);
    region.extend(obj_prop("USize", 2, None, &7i32.to_le_bytes()));
    region.push(0);
    button.extend(obj_prop("Region", 10, Some("Plane"), &region));
    button.extend(obj_prop("Materials", 9, None, &[2, 1, 2]));
    button.push(0);
    button.extend([0xAA, 0xBB, 0xCC]);
    let mut bar = obj_prop("Format", 1, None, &[3]);
    bar.push(0);
    let mut mix = obj_prop("Material1", 5, None, &[2]);
    mix.push(0);
    let broken = obj_prop("USize", 2, None, &256i32.to_le_bytes())[..3].to_vec();
    // Node, StateNode, ProbeMask, LatentAction.
    let mut odd = vec![0; 14];
    odd.extend(obj_prop("Scale", 4, None, &0.5f32.to_le_bytes()));
    odd.extend(obj_prop("Text", 13, None, &[3, b'h', b'i', 0]));
    odd.extend(obj_prop("OnClick", 7, None, &[0, n("OnClick") as u8]));
    let rotator: Vec<u8> = [1i32, 2, 3].iter().flat_map(|v| v.to_le_bytes()).collect();
    odd.extend(obj_prop("Rot", 10, Some("Rotator"), &rotator));
    odd.extend(obj_prop("Tint", 10, Some("Color"), &[1, 2, 3, 4]));
    odd.extend(obj_prop("Material1", 5, None, &[0]));
    let mut element = obj_prop("USize", 2, None, &7i32.to_le_bytes());
    element.push(0);
    odd.extend(obj_prop("Items", 9, None, &[&[1][..], &element].concat()));
    // A struct whose tag says it's a byte shorter than it is.
    let mut inner = obj_prop("Inner", 10, Some("Plane"), &element);
    inner[3] -= 1;
    odd.extend(inner);
    odd.extend(obj_prop("Blob", 14, None, &(0..40).collect::<Vec<u8>>()));
    odd.push(0);
    let flags = 0x00070004;
    package_file(
        OBJ_NAMES,
        &imports,
        &[
            (-5, 0, n("UI"), flags, vec![]),
            (-3, 1, n("Button"), flags, button),
            (-3, 1, n("Bar"), flags, bar),
            (-4, 0, n("Mix"), flags, mix),
            (0, 0, n("MyClass"), flags, class_data()),
            (-3, 0, n("Broken"), flags, broken),
            (-3, 0, n("Odd"), flags | 0x02000000, odd),
        ],
    )
}

/// A class with no fields or script, whose defaults set `USize=7`: its
/// `UField`, `UStruct`, `UState` and `UClass` data, then its defaults.
fn class_data() -> Vec<u8> {
    let mut data = vec![0, 0]; // SuperField, Next
    data.extend([0, 0]); // ScriptText, Children
    data.extend(compact_index(obj_name("MyClass"))); // FriendlyName
    data.extend(0u32.to_le_bytes()); // StructFlags
    data.extend(0i32.to_le_bytes()); // Line
    data.extend(0i32.to_le_bytes()); // TextPos
    data.extend(0i32.to_le_bytes()); // Script size
    data.extend([0; 8 + 8 + 2 + 4]); // ProbeMask, IgnoreMask, LabelTableOffset, StateFlags
    data.extend(0u32.to_le_bytes()); // ClassFlags
    data.extend([0; 16]); // ClassGuid
    data.extend([0, 0]); // Dependencies, PackageImports
    data.push(0); // ClassWithin
    data.extend(compact_index(obj_name("None"))); // ClassConfigName
    data.push(0); // HideCategories
    data.extend(obj_prop("USize", 2, None, &7i32.to_le_bytes()));
    data.push(0);
    data
}

fn obj_dir() -> TempDir {
    let dir = TempDir::new().unwrap();
    fs::write(dir.path().join("MyPkg.utx"), obj_package()).unwrap();
    dir
}

fn pkg_obj(dir: &Path, args: &[&str]) -> assert_cmd::assert::Assert {
    usagi(dir)
        .args(["pkg", "obj", "MyPkg.utx", "--color", "never"])
        .args(args)
        .assert()
}

const BUTTON_T3D: &str = "\
// Texture'MyPkg.UI.Button' (3 bytes of native data)
Begin Object Class=Texture Name=Button
    Format=5
    USize=256
    bMasked=True
    Tag(2)=Actor
    Pos=(X=1.000000,Y=2.500000,Z=-3.000000)
    Region=(Zone=Texture'Engine.DefaultTexture',USize=7)
    Materials=<2 elements, 2 bytes>
End Object
";

const BAR_T3D: &str = "\
// Texture'MyPkg.UI.Bar'
Begin Object Class=Texture Name=Bar
    Format=3
End Object
";

const MIX_T3D: &str = "\
// Combiner'MyPkg.Mix'
Begin Object Class=Combiner Name=Mix
    Material1=Texture'MyPkg.UI.Button'
End Object
";

#[test]
fn pkg_obj_prints_t3d() {
    let dir = obj_dir();
    pkg_obj(dir.path(), &["Texture'MyPkg.UI.Button'"])
        .code(0)
        .stdout(BUTTON_T3D);
    // Without the class, and in any case.
    pkg_obj(dir.path(), &["mypkg.ui.BUTTON"])
        .code(0)
        .stdout(BUTTON_T3D);
    // Several, once each, in table order.
    pkg_obj(dir.path(), &["MyPkg.Mix", "MyPkg.UI.*", "MyPkg.UI.Bar"])
        .code(0)
        .stdout(format!("{}\n{}\n{}", BUTTON_T3D, BAR_T3D, MIX_T3D));
    usagi(dir.path())
        .args(["pkg", "obj", "MyPkg.utx", "MyPkg.Mix", "--color", "always"])
        .assert()
        .code(0)
        .stdout(predicate::str::starts_with(
            "\x1b[2m// Combiner'MyPkg.Mix'\x1b[0m\n",
        ));
}

#[test]
fn pkg_obj_every_object() {
    let dir = obj_dir();
    let output = pkg_obj(dir.path(), &[])
        .code(1)
        .stderr(
            "usagi: MyPkg.utx: Texture'MyPkg.Broken': invalid data at offset 2: the data ends in \
             the property value\n",
        )
        .get_output()
        .stdout
        .clone();
    let text = String::from_utf8(output).unwrap();
    let expected = format!(
        "// Package'MyPkg.UI'\nBegin Object Class=Package Name=UI\nEnd Object\n\n{}\n{}\n{}\n\
         // Class'MyPkg.MyClass'\nBegin Object Class=Class Name=MyClass\n    USize=7\nEnd Object\n\n\
         // Texture'MyPkg.Broken'\nBegin Object Class=Texture Name=Broken\n    \
         // invalid data at offset 2: the data ends in the property value\nEnd Object\n\n{}",
        BUTTON_T3D, BAR_T3D, MIX_T3D, ODD_T3D
    );
    assert_eq!(text, expected);
}

#[test]
fn pkg_obj_classes() {
    let dir = obj_dir();
    pkg_obj(dir.path(), &["--class", "combiner"])
        .code(0)
        .stdout(MIX_T3D);
    pkg_obj(dir.path(), &["--class", "Engine.Combiner"])
        .code(0)
        .stdout(MIX_T3D);
    pkg_obj(
        dir.path(),
        &["--class", "Core.Package", "--format", "concise"],
    )
    .code(0)
    .stdout("");
    pkg_obj(dir.path(), &["--class", "Class"])
        .code(0)
        .stdout(predicate::str::starts_with("// Class'MyPkg.MyClass'\n"));
    pkg_obj(dir.path(), &["MyPkg.**", "--class", "Package,Combiner"])
        .code(0)
        .stdout(format!(
            "// Package'MyPkg.UI'\nBegin Object Class=Package Name=UI\nEnd Object\n\n{}",
            MIX_T3D
        ));
    // A class matches only its own objects, not a subclass's.
    pkg_obj(dir.path(), &["--class", "Object"])
        .code(0)
        .stdout("");
}

#[test]
fn pkg_obj_table() {
    let dir = obj_dir();
    pkg_obj(
        dir.path(),
        &["MyPkg.UI.*", "--property", "Format,usize,bMasked,Tag"],
    )
    .code(0)
    .stdout(
        "  Object                    Format  usize  bMasked  Tag
  Texture'MyPkg.UI.Button'  5       256    True     (2)=Actor
  Texture'MyPkg.UI.Bar'     3       -      -        -
",
    );
    // `?`: an object whose properties couldn't all be read.
    pkg_obj(dir.path(), &["MyPkg.Broken", "--property", "USize"])
        .code(1)
        .stdout("  Object                 USize\n  Texture'MyPkg.Broken'  ?\n");
}

#[test]
fn pkg_obj_concise() {
    let dir = obj_dir();
    pkg_obj(dir.path(), &["MyPkg.UI.*", "--format", "concise"])
        .code(0)
        .stdout(
            "Texture'MyPkg.UI.Button' Format=5
Texture'MyPkg.UI.Button' USize=256
Texture'MyPkg.UI.Button' bMasked=True
Texture'MyPkg.UI.Button' Tag(2)=Actor
Texture'MyPkg.UI.Button' Pos=(X=1.000000,Y=2.500000,Z=-3.000000)
Texture'MyPkg.UI.Button' Region=(Zone=Texture'Engine.DefaultTexture',USize=7)
Texture'MyPkg.UI.Button' Materials=<2 elements, 2 bytes>
Texture'MyPkg.UI.Bar' Format=3
",
        );
    pkg_obj(
        dir.path(),
        &["MyPkg.UI.*", "--format", "concise", "--property", "format"],
    )
    .code(0)
    .stdout("Texture'MyPkg.UI.Button' Format=5\nTexture'MyPkg.UI.Bar' Format=3\n");
}

#[test]
fn pkg_obj_json() {
    let dir = obj_dir();
    let output = pkg_obj(dir.path(), &["--format", "json"])
        .code(1)
        .get_output()
        .stdout
        .clone();
    let objects: serde_json::Value = serde_json::from_slice(&output).unwrap();
    let button = &objects[1];
    assert_eq!(button["index"], 2);
    assert_eq!(button["reference"], "Texture'MyPkg.UI.Button'");
    assert_eq!(button["class"], "Texture");
    assert_eq!(button["path"], "MyPkg.UI.Button");
    assert_eq!(button["size"], 55);
    assert_eq!(button["native_size"], 3);
    assert_eq!(button["error"], serde_json::Value::Null);
    let properties = &button["properties"];
    assert_eq!(
        properties[0],
        serde_json::json!({
            "name": "Format", "type": "ByteProperty", "index": 0, "offset": 0,
            "size": 1, "tag_size": 1, "value": 5
        })
    );
    assert_eq!(properties[2]["value"], true);
    assert_eq!(properties[3]["index"], 2);
    assert_eq!(properties[3]["value"], "Actor");
    assert_eq!(properties[4]["struct"], "Vector");
    assert_eq!(
        properties[4]["value"],
        serde_json::json!({ "X": 1.0, "Y": 2.5, "Z": -3.0 })
    );
    assert_eq!(
        properties[5]["value"][0]["value"],
        "Texture'Engine.DefaultTexture'"
    );
    assert_eq!(properties[5]["value"][1]["value"], 7);
    assert_eq!(
        properties[6]["value"],
        serde_json::json!({ "length": 2, "bytes": "0102" })
    );
    // A class: its defaults, after its own data. Cut short: those read, and
    // the error.
    assert_eq!(objects[4]["properties"][0]["name"], "USize");
    assert_eq!(objects[4]["properties"][0]["offset"], 68);
    assert_eq!(objects[4]["error"], serde_json::Value::Null);
    assert_eq!(objects[4]["native_size"], 0);
    assert_eq!(objects[5]["properties"], serde_json::json!([]));
    assert_eq!(
        objects[5]["error"],
        "invalid data at offset 2: the data ends in the property value"
    );
}

#[test]
fn pkg_obj_raw() {
    let dir = obj_dir();
    pkg_obj(dir.path(), &["MyPkg.UI", "MyPkg.UI.Button", "--raw"])
        .code(0)
        .stdout(
            "Package'MyPkg.UI': no data

Texture'MyPkg.UI.Button': 55 bytes at 0x1E8
  Offset  Name       Type                     Index  Size  Bytes
     0x0  Format     ByteProperty                 0     1  05
     0x3  USize      IntProperty                  0     4  00 01 00 00
     0x9  bMasked    BoolProperty                 0     0
     0xC  Tag        NameProperty                 2     1  19
    0x10  Pos        StructProperty (Vector)      0    12  00 00 80 3F 00 00 20 40 00 00 40 C0
    0x1F  Region     StructProperty (Plane)       0    10  11 05 86 0E 22 07 00 00 00 00
    0x2D  Materials  ArrayProperty                0     3  02 01 02
    0x33  None
  Native data  3 bytes at 0x34
",
        );
    pkg_obj(dir.path(), &["MyPkg.MyClass", "MyPkg.Broken", "--raw"])
        .code(1)
        .stdout(
            "Class'MyPkg.MyClass': 75 bytes at 0x227
  Class data  68 bytes
  Offset  Name   Type         Index  Size  Bytes
    0x44  USize  IntProperty      0     4  07 00 00 00
    0x4A  None

Texture'MyPkg.Broken': 3 bytes at 0x272
  invalid data at offset 2: the data ends in the property value
",
        );
    pkg_obj(dir.path(), &["--raw", "--format", "json"])
        .code(2)
        .stderr("usagi: --raw only works with --format text\n");
}

#[test]
fn pkg_obj_errors() {
    let dir = obj_dir();
    for (object, message) in [
        (
            "Shader'MyPkg.UI.Button'",
            "Shader'MyPkg.UI.Button' isn't a Shader: it's Texture'MyPkg.UI.Button'",
        ),
        ("MyPkg.Nope", "MyPkg.Nope isn't in MyPkg.utx"),
        (
            "MyPkg.Nope.*",
            "MyPkg.Nope.* matches no objects in MyPkg.utx",
        ),
        (
            "Texture'Engine.DefaultTexture'",
            "Texture'Engine.DefaultTexture' isn't in MyPkg.utx: it's imported from the package \
             Engine",
        ),
        (
            "Texture'MyPkg.Mix",
            "invalid object reference \"Texture'MyPkg.Mix\": it doesn't end with a '",
        ),
    ] {
        pkg_obj(dir.path(), &[object])
            .code(2)
            .stderr(format!("usagi: {}\n", message));
    }
    usagi(dir.path())
        .args(["pkg", "obj", "Missing.utx"])
        .assert()
        .code(2)
        .stderr(predicate::str::starts_with("usagi: Missing.utx: "));
}

const ODD_T3D: &str = "\
// Texture'MyPkg.Odd'
Begin Object Class=Texture Name=Odd
    Scale=0.500000
    Text=\"hi\"
    OnClick=None.None
    Rot=(Pitch=1,Yaw=2,Roll=3)
    Tint=(B=1,G=2,R=3,A=4)
    Material1=None
    Items=((USize=7))
    Inner=(USize=7)
    Blob=<40 bytes: 00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F ...>
End Object
";

#[test]
fn pkg_obj_other_types() {
    let dir = obj_dir();
    pkg_obj(dir.path(), &["MyPkg.Odd"]).code(0).stdout(ODD_T3D);
    // After the state frame; a struct whose tag has the wrong size; bytes
    // after the first 32 left out.
    pkg_obj(dir.path(), &["MyPkg.Odd", "--raw"]).code(0).stdout(
        "Texture'MyPkg.Odd': 121 bytes at 0x275
  State frame  14 bytes
  Offset  Name       Type                      Index        Size  Bytes
     0xE  Scale      FloatProperty                 0           4  00 00 00 3F
    0x14  Text       StrProperty                   0           4  03 68 69 00
    0x1A  OnClick    DelegateProperty              0           2  00 1D
    0x1E  Rot        StructProperty (Rotator)      0          12  01 00 00 00 02 00 00 00 03 00 00 00
    0x2D  Tint       StructProperty (Color)        0           4  01 02 03 04
    0x34  Material1  ObjectProperty                0           1  00
    0x37  Items      ArrayProperty                 0           8  01 0E 22 07 00 00 00 00
    0x42  Inner      StructProperty (Plane)        0  7 (tag: 6)  0E 22 07 00 00 00 00
    0x4D  Blob       MapProperty                   0          40  00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F 10 11 12 13 14 15 16 17 18 19 1A 1B 1C 1D 1E 1F ...
    0x78  None
",
    );
    let output = pkg_obj(dir.path(), &["MyPkg.Odd", "--format", "json"])
        .code(0)
        .get_output()
        .stdout
        .clone();
    let objects: serde_json::Value = serde_json::from_slice(&output).unwrap();
    let values: Vec<&serde_json::Value> = objects[0]["properties"]
        .as_array()
        .unwrap()
        .iter()
        .map(|p| &p["value"])
        .collect();
    assert_eq!(
        serde_json::Value::Array(values.into_iter().cloned().collect()),
        serde_json::json!([
            0.5,
            "hi",
            { "object": null, "function": "OnClick" },
            { "Pitch": 1, "Yaw": 2, "Roll": 3 },
            { "B": 1, "G": 2, "R": 3, "A": 4 },
            null,
            [[{ "name": "USize", "type": "IntProperty", "index": 0, "offset": 59,
                "size": 4, "tag_size": 4, "value": 7 }]],
            [{ "name": "USize", "type": "IntProperty", "index": 0, "offset": 70,
               "size": 4, "tag_size": 4, "value": 7 }],
            { "bytes": (0..40u8).map(|b| format!("{:02x}", b)).collect::<String>() },
        ])
    );
    assert_eq!(objects[0]["properties"][7]["tag_size"], 6);
}

fn pkg_export(dir: &Path, args: &[&str]) -> assert_cmd::assert::Assert {
    usagi(dir).args(["pkg", "export"]).args(args).assert()
}

/// The files under `dir`, relative to it, sorted, with `/` between folders.
fn files_under(dir: &Path) -> Vec<String> {
    fn walk(root: &Path, dir: &Path, out: &mut Vec<String>) {
        for entry in fs::read_dir(dir).unwrap() {
            let path = entry.unwrap().path();
            if path.is_dir() {
                walk(root, &path, out);
            } else {
                let relative = path.strip_prefix(root).unwrap();
                let parts: Vec<_> = relative.iter().map(|p| p.to_string_lossy()).collect();
                out.push(parts.join("/"));
            }
        }
    }
    let mut out = Vec::new();
    walk(dir, dir, &mut out);
    out.sort();
    out
}

#[test]
fn pkg_export_writes_native_data_and_t3d() {
    let dir = obj_dir();
    pkg_export(
        dir.path(),
        &[
            "MyPkg.utx",
            "--object",
            "Texture'MyPkg.UI.Button',MyPkg.Mix",
            "-o",
            "out",
        ],
    )
    .code(0)
    .stdout("Exported 2 objects to out\n")
    .stderr("");
    let out = dir.path().join("out");
    assert_eq!(
        files_under(&out),
        [
            "Combiner'MyPkg.Mix'.bin",
            "Combiner'MyPkg.Mix'.t3d",
            "Texture'MyPkg.UI.Button'.bin",
            "Texture'MyPkg.UI.Button'.t3d",
        ]
    );
    let button = |extension: &str| out.join(format!("Texture'MyPkg.UI.Button'.{}", extension));
    assert_eq!(fs::read(button("bin")).unwrap(), [0xAA, 0xBB, 0xCC]);
    assert_eq!(fs::read_to_string(button("t3d")).unwrap(), BUTTON_T3D);
    // Parts with nothing in them are written anyway.
    assert_eq!(
        fs::read(out.join("Combiner'MyPkg.Mix'.bin")).unwrap(),
        Vec::<u8>::new()
    );
    assert_eq!(
        fs::read_to_string(out.join("Combiner'MyPkg.Mix'.t3d")).unwrap(),
        MIX_T3D
    );

    // Files already there are overwritten.
    fs::write(button("bin"), "old").unwrap();
    pkg_export(
        dir.path(),
        &[
            "MyPkg.utx",
            "--object",
            "MyPkg.UI.Button",
            "--output",
            "out",
        ],
    )
    .code(0)
    .stdout("Exported 1 object to out\n");
    assert_eq!(fs::read(button("bin")).unwrap(), [0xAA, 0xBB, 0xCC]);
}

#[test]
fn pkg_export_every_object() {
    let dir = obj_dir();
    // Not the group, UI. A class's .t3d is its defaults.
    pkg_export(dir.path(), &["MyPkg.utx", "-o", "out"])
        .code(1)
        .stdout("Exported 5 objects to out\n")
        .stderr(
            "usagi: MyPkg.utx: Texture'MyPkg.Broken': invalid data at offset 2: the data ends in \
             the property value\n",
        );
    let out = dir.path().join("out");
    assert_eq!(files_under(&out).len(), 10);
    assert_eq!(
        fs::read_to_string(out.join("Class'MyPkg.MyClass'.t3d")).unwrap(),
        "// Class'MyPkg.MyClass'\nBegin Object Class=Class Name=MyClass\n    USize=7\nEnd Object\n"
    );
    assert_eq!(
        fs::read_to_string(out.join("Texture'MyPkg.Odd'.t3d")).unwrap(),
        ODD_T3D
    );
    // A group asked for by name isn't an error.
    pkg_export(dir.path(), &["MyPkg.utx", "--class", "Class", "-o", "x"])
        .code(0)
        .stdout("Exported 1 object to x\n");
    pkg_export(
        dir.path(),
        &["MyPkg.utx", "--object", "MyPkg.UI", "-o", "x"],
    )
    .code(0)
    .stdout("Exported 0 objects to x\n");
}

#[test]
fn pkg_export_folder_layout() {
    let dir = obj_dir();
    pkg_export(
        dir.path(),
        &[
            "MyPkg.utx",
            "--object",
            "MyPkg.**",
            "-o",
            "out",
            "--layout",
            "folder",
        ],
    )
    .code(1);
    assert_eq!(
        files_under(&dir.path().join("out")),
        [
            "MyPkg/Class/MyClass.bin",
            "MyPkg/Class/MyClass.t3d",
            "MyPkg/Combiner/Mix.bin",
            "MyPkg/Combiner/Mix.t3d",
            "MyPkg/Texture/Odd.bin",
            "MyPkg/Texture/Odd.t3d",
            "MyPkg/Texture/UI/Bar.bin",
            "MyPkg/Texture/UI/Bar.t3d",
            "MyPkg/Texture/UI/Button.bin",
            "MyPkg/Texture/UI/Button.t3d",
        ]
    );
}

#[test]
fn pkg_export_several_packages() {
    let dir = obj_dir();
    fs::write(dir.path().join("Other.utx"), obj_package()).unwrap();
    // A pattern only has to match in one of them.
    pkg_export(
        dir.path(),
        &[
            "MyPkg.utx",
            "Other.utx",
            "--object",
            "Other.UI.Bar,MyPkg.Mix",
            "-o",
            "out",
        ],
    )
    .code(0)
    .stdout("Exported 2 objects to out\n");
    assert_eq!(
        files_under(&dir.path().join("out")),
        [
            "Combiner'MyPkg.Mix'.bin",
            "Combiner'MyPkg.Mix'.t3d",
            "Texture'Other.UI.Bar'.bin",
            "Texture'Other.UI.Bar'.t3d",
        ]
    );
    // The same package twice: nothing is written.
    fs::create_dir(dir.path().join("copy")).unwrap();
    fs::write(dir.path().join("copy/MyPkg.utx"), obj_package()).unwrap();
    let args = [
        "MyPkg.utx",
        "copy/MyPkg.utx",
        "--object",
        "MyPkg.Mix",
        "-o",
        "x",
    ];
    pkg_export(dir.path(), &args).code(2).stderr(
        "usagi: Combiner'MyPkg.Mix' and Combiner'MyPkg.Mix' would both be written to \
             Combiner'MyPkg.Mix'\n",
    );
    pkg_export(dir.path(), &[&args[..], &["--layout", "folder"]].concat())
        .code(2)
        .stderr(format!(
            "usagi: Combiner'MyPkg.Mix' and Combiner'MyPkg.Mix' would both be written to {}\n",
            Path::new("MyPkg").join("Combiner").join("Mix").display()
        ));
    assert!(!dir.path().join("x").exists());
}
#[test]
fn pkg_export_errors() {
    let dir = obj_dir();
    fs::write(dir.path().join("Other.utx"), obj_package()).unwrap();
    let two = ["MyPkg.utx", "Other.utx", "-o", "out", "--object"];
    for (args, message) in [
        (
            &["MyPkg.utx", "-o", "out", "--object", "MyPkg.Nope"][..],
            "MyPkg.Nope isn't in MyPkg.utx",
        ),
        (
            &[&two[..], &["MyPkg.Nope"]].concat(),
            "MyPkg.Nope isn't in any of the 2 packages",
        ),
        (
            &[&two[..], &["*.Nope.*"]].concat(),
            "*.Nope.* matches no objects in the 2 packages",
        ),
        (
            &[&two[..], &["Shader'Other.UI.Bar'"]].concat(),
            "Shader'Other.UI.Bar' isn't a Shader: it's Texture'Other.UI.Bar'",
        ),
        (
            &[&two[..], &["Texture'MyPkg.Mix"]].concat(),
            "invalid object reference \"Texture'MyPkg.Mix\": it doesn't end with a '",
        ),
    ] {
        pkg_export(dir.path(), args)
            .code(2)
            .stderr(format!("usagi: {}\n", message));
    }
    pkg_export(dir.path(), &["Missing.utx", "-o", "out"])
        .code(2)
        .stderr(predicate::str::starts_with("usagi: Missing.utx: "));
    // -o is required, and so is a package.
    pkg_export(dir.path(), &["MyPkg.utx"]).code(2);
    pkg_export(dir.path(), &["-o", "out"]).code(2);
    // The output can't be written.
    fs::write(dir.path().join("file"), "").unwrap();
    pkg_export(
        dir.path(),
        &["MyPkg.utx", "--object", "MyPkg.Mix", "-o", "file"],
    )
    .code(2)
    .stderr(predicate::str::starts_with("usagi: file"));
    assert!(!dir.path().join("out").exists());
}

/// `MyPkg.u` for `pkg diff`, old or new: a group `UI`, a combiner `UI.Bar`
/// whose properties and native data change, one `Mix` that doesn't, and one
/// removed (`Button`) and one added (`Odd`). The new one also has the flag
/// `ServerSideOnly`, imports `Engine.Actor` rather than `Engine.Texture`,
/// and depends on `Zone`.
fn diff_package(new: bool) -> Vec<u8> {
    let n = obj_name;
    let mut imports = vec![
        (n("Core"), n("Package"), 0, n("Core")),
        (n("Core"), n("Package"), 0, n("Engine")),
        (n("Core"), n("Class"), -2, n("Combiner")),
        (n("Core"), n("Class"), -1, n("Package")),
    ];
    if new {
        imports.push((n("Core"), n("Class"), -2, n("Actor")));
        imports.push((n("Core"), n("Package"), 0, n("Zone")));
    } else {
        imports.push((n("Core"), n("Class"), -2, n("Texture")));
    }
    let properties = |props: &[Vec<u8>], native: &[u8]| {
        let mut data = props.concat();
        data.push(0);
        data.extend(native);
        data
    };
    let flags = 0x00070004;
    let bar = if new {
        properties(
            &[
                obj_prop("Format", 1, None, &[5]),
                obj_prop("USize", 2, None, &256i32.to_le_bytes()),
            ],
            &[1, 2, 3],
        )
    } else {
        properties(&[obj_prop("Format", 1, None, &[3])], &[1, 2])
    };
    let mix = properties(&[obj_prop("USize", 2, None, &7i32.to_le_bytes())], &[]);
    let mut exports = vec![
        (-4, 0, n("UI"), flags, vec![]),
        (-3, 1, n("Bar"), flags, bar),
        (-3, 0, n("Mix"), flags, mix),
    ];
    if new {
        exports.push((-3, 0, n("Odd"), flags, properties(&[], &[0; 10])));
    } else {
        exports.insert(0, (-3, 0, n("Button"), flags, properties(&[], &[0; 4])));
        // The group is now the second export.
        for export in &mut exports[2..] {
            if export.1 == 1 {
                export.1 = 2;
            }
        }
    }
    let mut bytes = package_file(OBJ_NAMES, &imports, &exports);
    if new {
        bytes[8..12].copy_from_slice(&0x5u32.to_le_bytes());
    }
    bytes
}

fn diff_dir() -> TempDir {
    let dir = TempDir::new().unwrap();
    fs::create_dir(dir.path().join("old")).unwrap();
    fs::write(dir.path().join("old/MyPkg.u"), diff_package(false)).unwrap();
    fs::write(dir.path().join("MyPkg.u"), diff_package(true)).unwrap();
    dir
}

fn pkg_diff(dir: &Path, args: &[&str]) -> assert_cmd::assert::Assert {
    usagi(dir)
        .args(["pkg", "diff", "old/MyPkg.u", "MyPkg.u", "--color", "never"])
        .args(args)
        .assert()
}

const DIFF_TEXT: &str = "\
--- old/MyPkg.u
+++ MyPkg.u
Summary
  Flags        0x00000001 (AllowDownload) -> 0x00000005 (AllowDownload, ServerSideOnly)
  Size         595 -> 615 bytes (+20)
Dependencies
  + Zone
Imports
  + Class'Engine.Actor'
  + Package'Zone'
  - Class'Engine.Texture'
Exports
  - Combiner'MyPkg.Button' (5 bytes)
  + Combiner'MyPkg.Odd' (11 bytes)
  ~ Combiner'MyPkg.UI.Bar'
      Size         6 -> 13 bytes (+7)
      ~ Format=3 -> 5
      + USize=256
      Native data  2 -> 3 bytes (+1)
";

#[test]
fn pkg_diff_text() {
    let dir = diff_dir();
    pkg_diff(dir.path(), &[]).code(0).stdout(DIFF_TEXT);
    pkg_diff(dir.path(), &["--exit-code"])
        .code(1)
        .stdout(DIFF_TEXT);
    // Colour.
    let out = usagi(dir.path())
        .args(["pkg", "diff", "old/MyPkg.u", "MyPkg.u", "--color", "always"])
        .assert()
        .code(0)
        .get_output()
        .stdout
        .clone();
    let out = String::from_utf8(out).unwrap();
    assert!(out.contains("\x1b[32m+\x1b[0m Zone"), "{}", out);
    assert!(
        out.contains("\x1b[31m-\x1b[0m Combiner'MyPkg.Button'"),
        "{}",
        out
    );
    assert!(
        out.contains("\x1b[33m~\x1b[0m Combiner'MyPkg.UI.Bar'"),
        "{}",
        out
    );
}

#[test]
fn pkg_diff_identical_packages() {
    let dir = diff_dir();
    for args in [&[][..], &["--all"], &["--format", "concise"]] {
        usagi(dir.path())
            .args(["pkg", "diff", "MyPkg.u", "MyPkg.u", "--exit-code"])
            .args(args)
            .assert()
            .code(0)
            .stdout("");
    }
    let out = usagi(dir.path())
        .args(["pkg", "diff", "MyPkg.u", "MyPkg.u", "--format", "json"])
        .assert()
        .code(0)
        .get_output()
        .stdout
        .clone();
    let json: serde_json::Value = serde_json::from_slice(&out).unwrap();
    assert_eq!(json["summary"], serde_json::json!({}));
    assert_eq!(json["exports"], serde_json::json!([]));
}

#[test]
fn pkg_diff_concise() {
    let dir = diff_dir();
    pkg_diff(dir.path(), &["--format", "concise"])
        .code(0)
        .stdout(
            "\
~ Flags: 0x00000001 (AllowDownload) -> 0x00000005 (AllowDownload, ServerSideOnly)
~ Size: 595 -> 615 bytes (+20)
+ Dependencies: Zone
+ Imports: Class'Engine.Actor'
+ Imports: Package'Zone'
- Imports: Class'Engine.Texture'
- Combiner'MyPkg.Button'
+ Combiner'MyPkg.Odd'
~ Combiner'MyPkg.UI.Bar' Size: 6 -> 13 bytes (+7)
~ Combiner'MyPkg.UI.Bar' Format=3 -> 5
+ Combiner'MyPkg.UI.Bar' USize=256
~ Combiner'MyPkg.UI.Bar' Native data: 2 -> 3 bytes (+1)
",
        );
}

#[test]
fn pkg_diff_json() {
    let dir = diff_dir();
    let out = pkg_diff(dir.path(), &["--format", "json"])
        .code(0)
        .get_output()
        .stdout
        .clone();
    let json: serde_json::Value = serde_json::from_slice(&out).unwrap();
    assert_eq!(json["old"], "old/MyPkg.u");
    assert_eq!(json["summary"]["package_flags"]["new"]["bits"], 5);
    assert_eq!(
        json["summary"]["size"],
        serde_json::json!({ "old": 595, "new": 615, "delta": 20 })
    );
    assert_eq!(json["dependencies"]["added"], serde_json::json!(["Zone"]));
    assert_eq!(
        json["imports"]["removed"],
        serde_json::json!(["Class'Engine.Texture'"])
    );
    let exports = json["exports"].as_array().unwrap();
    assert_eq!(exports.len(), 3);
    assert_eq!(exports[0]["status"], "removed");
    assert_eq!(
        exports[0]["size"],
        serde_json::json!({ "old": 5, "new": null, "delta": -5 })
    );
    assert_eq!(exports[1]["status"], "added");
    assert_eq!(exports[1]["new_index"], 4);
    let bar = &exports[2];
    assert_eq!(bar["status"], "changed");
    assert_eq!(
        (&bar["old_index"], &bar["new_index"]),
        (&3.into(), &2.into())
    );
    assert_eq!(
        bar["properties"],
        serde_json::json!([
            { "name": "Format", "old": "3", "new": "5", "volatile": null },
            { "name": "USize", "old": null, "new": "256", "volatile": null },
        ])
    );
    assert_eq!(
        bar["native"],
        serde_json::json!({ "old": 2, "new": 3, "delta": 1, "changed": true })
    );
    assert!(bar.get("offset").is_none());
    // With --all, what changes on every save.
    let out = pkg_diff(dir.path(), &["--format", "json", "--all"])
        .get_output()
        .stdout
        .clone();
    let json: serde_json::Value = serde_json::from_slice(&out).unwrap();
    assert_eq!(json["summary"]["exports"]["new"]["offset"], 0x231);
    assert_eq!(json["exports"].as_array().unwrap().len(), 5);
    assert_eq!(
        json["exports"][4]["offset"],
        serde_json::json!({ "old": 0x1ED, "new": 0x1E8 })
    );
}

#[test]
fn pkg_diff_all_and_humanize() {
    let dir = diff_dir();
    let out = pkg_diff(dir.path(), &["--all", "--humanize"])
        .code(0)
        .get_output()
        .stdout
        .clone();
    let out = String::from_utf8(out).unwrap();
    for line in [
        "  Size         595 B -> 615 B (+20 B)\n",
        "  Imports      5 at 0x1FA -> 6 at 0x207\n",
        "  ~ Combiner'MyPkg.Mix'\n      Index        4 -> 3\n      Offset       0x1F3 -> 0x1F5\n",
        "  ~ Package'MyPkg.UI'\n      Index        2 -> 1\n",
        "  + Combiner'MyPkg.Odd' (11 B)\n",
        "      Native data  2 B -> 3 B (+1 B)\n",
    ] {
        assert!(out.contains(line), "{:?} isn't in\n{}", line, out);
    }
    // Only the save changed: nothing without --all.
    let dir = TempDir::new().unwrap();
    let old = diff_package(true);
    let mut new = old.clone();
    // The GUID.
    new[36] ^= 0xFF;
    fs::write(dir.path().join("Old.u"), old).unwrap();
    fs::write(dir.path().join("New.u"), new).unwrap();
    let diff = |args: &[&str]| {
        usagi(dir.path())
            .args([
                "pkg",
                "diff",
                "Old.u",
                "New.u",
                "--exit-code",
                "--color",
                "never",
            ])
            .args(args)
            .assert()
    };
    diff(&[]).code(0).stdout("");
    diff(&["--all"])
        .code(1)
        .stdout(predicate::str::contains("  GUID  "));
}

#[test]
fn pkg_diff_objects_and_classes() {
    let dir = diff_dir();
    // Only the objects: no summary, dependencies or imports.
    pkg_diff(dir.path(), &["--object", "MyPkg.UI.**", "--exit-code"])
        .code(1)
        .stdout(
            "\
--- old/MyPkg.u
+++ MyPkg.u
Exports
  ~ Combiner'MyPkg.UI.Bar'
      Size         6 -> 13 bytes (+7)
      ~ Format=3 -> 5
      + USize=256
      Native data  2 -> 3 bytes (+1)
",
        );
    pkg_diff(dir.path(), &["--object", "MyPkg.Mix", "--exit-code"])
        .code(0)
        .stdout("");
    // In only one of them.
    pkg_diff(
        dir.path(),
        &["--object", "MyPkg.Button", "--format", "concise"],
    )
    .code(0)
    .stdout("- Combiner'MyPkg.Button'\n");
    pkg_diff(
        dir.path(),
        &["--class", "Package", "--all", "--format", "concise"],
    )
    .code(0)
    .stdout("~ Package'MyPkg.UI' Index: 2 -> 1\n");
    let out = pkg_diff(dir.path(), &["--class", "Package", "--format", "json"])
        .get_output()
        .stdout
        .clone();
    let json: serde_json::Value = serde_json::from_slice(&out).unwrap();
    assert!(json.get("summary").is_none() && json.get("imports").is_none());
    pkg_diff(dir.path(), &["--object", "MyPkg.Nope"])
        .code(2)
        .stderr(predicate::str::contains("MyPkg.Nope isn't in MyPkg.u"));
    pkg_diff(dir.path(), &["--object", "Bad'"]).code(2);
}

#[test]
fn pkg_diff_errors() {
    let dir = diff_dir();
    usagi(dir.path())
        .args(["pkg", "diff", "Missing.u", "MyPkg.u"])
        .assert()
        .code(2)
        .stderr(predicate::str::contains("Missing.u"));
    fs::write(dir.path().join("Junk.u"), b"junk").unwrap();
    usagi(dir.path())
        .args(["pkg", "diff", "MyPkg.u", "Junk.u"])
        .assert()
        .code(2);
    // An object whose properties are cut short, in the new package.
    let n = obj_name;
    let imports = [
        (n("Core"), n("Package"), 0, n("Engine")),
        (n("Core"), n("Class"), -1, n("Combiner")),
    ];
    let package =
        |data: Vec<u8>| package_file(OBJ_NAMES, &imports, &[(-2, 0, n("Mix"), 0x00070004, data)]);
    fs::write(dir.path().join("Good.u"), package(vec![0])).unwrap();
    let cut = obj_prop("USize", 2, None, &7i32.to_le_bytes())[..3].to_vec();
    fs::write(dir.path().join("Cut.u"), package(cut)).unwrap();
    let out = usagi(dir.path())
        .args(["pkg", "diff", "Good.u", "Cut.u", "--color", "never"])
        .assert()
        .code(1)
        .get_output()
        .clone();
    let stdout = String::from_utf8(out.stdout).unwrap();
    let stderr = String::from_utf8(out.stderr).unwrap();
    assert!(stdout.contains("      Error (new): "), "{}", stdout);
    assert!(
        stderr.starts_with("usagi: Cut.u: Combiner'Cut.Mix': "),
        "{}",
        stderr
    );
    let out = usagi(dir.path())
        .args(["pkg", "diff", "Cut.u", "Good.u", "--format", "concise"])
        .assert()
        .code(1)
        .get_output()
        .stdout
        .clone();
    let concise = String::from_utf8(out).unwrap();
    assert!(
        concise.contains("\n! Combiner'Good.Mix' Error (old): invalid data at offset 2"),
        "{}",
        concise
    );
}

fn git(dir: &Path, args: &[&str]) {
    let status = std::process::Command::new("git")
        .args([
            "-c",
            "user.name=usagi",
            "-c",
            "user.email=usagi@example.com",
        ])
        .args(args)
        .current_dir(dir)
        .output()
        .unwrap()
        .status;
    assert!(status.success(), "git {:?}", args);
}

#[test]
fn pkg_diff_git_revisions() {
    if std::process::Command::new("git")
        .arg("--version")
        .output()
        .is_err()
    {
        return;
    }
    let dir = TempDir::new().unwrap();
    let system = dir.path().join("System");
    fs::create_dir(&system).unwrap();
    fs::write(system.join("MyPkg.u"), diff_package(false)).unwrap();
    git(dir.path(), &["init", "-q"]);
    git(dir.path(), &["add", "."]);
    git(dir.path(), &["commit", "-q", "-m", "old"]);
    fs::write(system.join("MyPkg.u"), diff_package(true)).unwrap();
    let expected = DIFF_TEXT
        .replace("--- old/MyPkg.u", "--- HEAD:System/MyPkg.u")
        .replace("+++ MyPkg.u", "+++ System/MyPkg.u");
    usagi(dir.path())
        .args([
            "pkg",
            "diff",
            "HEAD:System/MyPkg.u",
            "System/MyPkg.u",
            "--color",
            "never",
        ])
        .assert()
        .code(0)
        .stdout(expected);
    // From a folder below the top: relative to it with `./`, as git reads
    // it.
    let expected = DIFF_TEXT
        .replace("--- old/MyPkg.u", "--- HEAD:./MyPkg.u")
        .replace("+++ MyPkg.u", "+++ MyPkg.u");
    usagi(&system)
        .args([
            "pkg",
            "diff",
            "HEAD:./MyPkg.u",
            "MyPkg.u",
            "--color",
            "never",
        ])
        .assert()
        .code(0)
        .stdout(expected);
    // Both in git.
    git(dir.path(), &["commit", "-q", "-am", "new"]);
    usagi(&system)
        .args([
            "pkg",
            "diff",
            "HEAD~:System/MyPkg.u",
            "HEAD:System/MyPkg.u",
            "--exit-code",
        ])
        .assert()
        .code(1);
    // A revision or path that isn't there.
    usagi(dir.path())
        .args(["pkg", "diff", "nope:System/MyPkg.u", "System/MyPkg.u"])
        .assert()
        .code(2)
        .stderr(predicate::str::contains("usagi: nope:System/MyPkg.u: "));
    // Not a package.
    fs::write(dir.path().join("Junk.u"), b"junk").unwrap();
    git(dir.path(), &["add", "Junk.u"]);
    git(dir.path(), &["commit", "-q", "-m", "junk"]);
    usagi(dir.path())
        .args(["pkg", "diff", "HEAD:Junk.u", "System/MyPkg.u"])
        .assert()
        .code(2)
        .stderr(predicate::str::contains("HEAD:Junk.u: "));
}

#[test]
fn pkg_diff_compressed_packages_from_git() {
    if std::process::Command::new("git")
        .arg("--version")
        .output()
        .is_err()
    {
        return;
    }
    let dir = TempDir::new().unwrap();
    fs::write(dir.path().join("MyPkg.u"), diff_package(true)).unwrap();
    fs::write(
        dir.path().join("MyPkg.u.uz2"),
        uz2_file(&diff_package(true)),
    )
    .unwrap();
    git(dir.path(), &["init", "-q"]);
    git(dir.path(), &["add", "."]);
    git(dir.path(), &["commit", "-q", "-m", "packages"]);
    usagi(dir.path())
        .args([
            "pkg",
            "diff",
            "HEAD:MyPkg.u.uz2",
            "MyPkg.u",
            "--format",
            "concise",
        ])
        .assert()
        .code(0)
        .stdout(predicate::str::starts_with("~ Compressed: "));
}

/// A git repository with `System/MyPkg.u` committed twice (old, then new),
/// then changed back to the old one in the working tree.
fn diff_repo() -> Option<TempDir> {
    std::process::Command::new("git")
        .arg("--version")
        .output()
        .ok()?;
    let dir = TempDir::new().unwrap();
    let system = dir.path().join("System");
    fs::create_dir(&system).unwrap();
    fs::write(system.join("MyPkg.u"), diff_package(false)).unwrap();
    git(dir.path(), &["init", "-q"]);
    git(dir.path(), &["add", "."]);
    git(dir.path(), &["commit", "-q", "-m", "old"]);
    git(dir.path(), &["tag", "old"]);
    fs::write(system.join("MyPkg.u"), diff_package(true)).unwrap();
    git(dir.path(), &["commit", "-q", "-am", "new"]);
    fs::write(system.join("MyPkg.u"), diff_package(false)).unwrap();
    Some(dir)
}

/// `DIFF_TEXT` with these names for the old and new packages.
fn diff_text(old: &str, new: &str) -> String {
    DIFF_TEXT
        .replace("--- old/MyPkg.u", &format!("--- {}", old))
        .replace("+++ MyPkg.u", &format!("+++ {}", new))
}

#[test]
fn pkg_diff_revisions_and_a_path() {
    let Some(dir) = diff_repo() else { return };
    let diff = |cwd: &Path, args: &[&str]| {
        usagi(cwd)
            .args(["pkg", "diff", "--color", "never"])
            .args(args)
            .assert()
    };
    let root = dir.path();
    // Two revisions.
    diff(root, &["old", "HEAD", "System/MyPkg.u"])
        .code(0)
        .stdout(diff_text("old:System/MyPkg.u", "HEAD:System/MyPkg.u"));
    // A revision and the working tree, which is the old one again.
    diff(root, &["old", "System/MyPkg.u", "--exit-code"])
        .code(0)
        .stdout("");
    // HEAD and the working tree.
    let reversed = diff(root, &["System/MyPkg.u", "--format", "concise"])
        .code(0)
        .get_output()
        .stdout
        .clone();
    let reversed = String::from_utf8(reversed).unwrap();
    assert!(
        reversed.contains("+ Combiner'MyPkg.Button'\n"),
        "{}",
        reversed
    );
    // From a folder below the top, and with `..`; or an absolute path.
    let system = root.join("System");
    diff(&system, &["HEAD~", "HEAD", "MyPkg.u"])
        .code(0)
        .stdout(diff_text("HEAD~:System/MyPkg.u", "HEAD:System/MyPkg.u"));
    diff(&system, &["HEAD~", "HEAD", "../System/./MyPkg.u"])
        .code(0)
        .stdout(diff_text("HEAD~:System/MyPkg.u", "HEAD:System/MyPkg.u"));
    let absolute = system.join("MyPkg.u");
    diff(&system, &["old", "HEAD", absolute.to_str().unwrap()])
        .code(0)
        .stdout(diff_text("old:System/MyPkg.u", "HEAD:System/MyPkg.u"));
    // `--`: everything before it is a revision, even a file's name.
    fs::write(root.join("old"), "not a package").unwrap();
    diff(root, &["old", "HEAD", "--", "System/MyPkg.u"])
        .code(0)
        .stdout(diff_text("old:System/MyPkg.u", "HEAD:System/MyPkg.u"));
    diff(root, &["--exit-code", "--", "System/MyPkg.u"]).code(1);
    // Two files are still two files.
    fs::create_dir(root.join("New")).unwrap();
    fs::write(root.join("New/MyPkg.u"), diff_package(true)).unwrap();
    diff(root, &["System/MyPkg.u", "New/MyPkg.u"])
        .code(0)
        .stdout(diff_text("System/MyPkg.u", "New/MyPkg.u"));
}

#[test]
fn pkg_diff_revision_errors() {
    let Some(dir) = diff_repo() else { return };
    let diff = |args: &[&str]| usagi(dir.path()).args(["pkg", "diff"]).args(args).assert();
    diff(&[])
        .code(2)
        .stderr(predicate::str::contains("a package's path is needed"));
    diff(&["nope", "System/MyPkg.u"])
        .code(2)
        .stderr(predicate::str::contains(
            "nope is neither a file nor a git revision",
        ));
    diff(&["old", "HEAD", "HEAD", "System/MyPkg.u"])
        .code(2)
        .stderr(predicate::str::contains("at most two revisions"));
    diff(&["old", "System/Gone.u"])
        .code(2)
        .stderr(predicate::str::contains("System/Gone.u isn't in old"));
    diff(&["old", "HEAD", "../Elsewhere.u"])
        .code(2)
        .stderr(predicate::str::contains(
            "../Elsewhere.u is outside the repository",
        ));
    diff(&["old", "HEAD", "/Elsewhere.u"])
        .code(2)
        .stderr(predicate::str::contains(
            "/Elsewhere.u is outside the repository",
        ));
    // Not in a repository.
    let outside = TempDir::new().unwrap();
    usagi(outside.path())
        .args(["pkg", "diff", "MyPkg.u"])
        .env("GIT_CEILING_DIRECTORIES", outside.path().parent().unwrap())
        .assert()
        .code(2)
        .stderr(predicate::str::contains(
            "MyPkg.u: fatal: not a git repository",
        ));
}

#[test]
fn pkg_diff_sizes_that_changed_only_in_encoding() {
    // `Holder` refers to `Target`, whose index needs a byte more in the new
    // package, behind 70 other objects.
    let package = |padding: usize| {
        let pads: Vec<String> = (0..padding).map(|i| format!("Pad{}", i)).collect();
        let mut names = vec![
            "None",
            "Core",
            "Package",
            "Engine",
            "Class",
            "Combiner",
            "Material1",
        ];
        names.extend(["Target", "Holder"]);
        names.extend(pads.iter().map(String::as_str));
        let n = |text: &str| names.iter().position(|&n| n == text).unwrap() as i32;
        let imports = [
            (n("Core"), n("Package"), 0, n("Engine")),
            (n("Core"), n("Class"), -1, n("Combiner")),
        ];
        let flags = 0x00070004;
        let mut exports: Vec<_> = pads
            .iter()
            .map(|pad| (-2, 0, n(pad), flags, vec![0]))
            .collect();
        let target = exports.len() as i32 + 1;
        exports.push((-2, 0, n("Target"), flags, vec![0]));
        let mut holder = compact_index(n("Material1"));
        let reference = compact_index(target);
        holder.push(5 | if reference.len() == 1 { 0x00 } else { 0x10 });
        holder.extend(reference);
        holder.push(0);
        exports.push((-2, 0, n("Holder"), flags, holder));
        package_file(&names, &imports, &exports)
    };
    let dir = TempDir::new().unwrap();
    fs::write(dir.path().join("Old.u"), package(0)).unwrap();
    fs::write(dir.path().join("New.u"), package(70)).unwrap();
    let diff = |args: &[&str]| {
        usagi(dir.path())
            .args([
                "pkg", "diff", "Old.u", "New.u", "--object", "*.Holder", "--color", "never",
            ])
            .args(args)
            .assert()
    };
    diff(&["--exit-code"]).code(0).stdout("");
    diff(&["--all", "--format", "concise"]).code(0).stdout(
        "~ Combiner'New.Holder' Size: 4 -> 5 bytes (+1)\n\
         ~ Combiner'New.Holder' Index: 2 -> 72\n\
         ~ Combiner'New.Holder' Offset: 0xAE -> 0x3EC\n",
    );
    let out = diff(&["--all", "--format", "json"])
        .get_output()
        .stdout
        .clone();
    let json: serde_json::Value = serde_json::from_slice(&out).unwrap();
    let holder = &json["exports"][0];
    assert_eq!(holder["size"], serde_json::Value::Null);
    assert_eq!(
        holder["encoded_size"],
        serde_json::json!({ "old": 4, "new": 5, "delta": 1 })
    );
}

#[test]
fn pkg_diff_volatile_properties() {
    // A texture panner whose `M` (as an int, for brevity) changed.
    let package = |m: i32| {
        let names = [
            "None",
            "Core",
            "Package",
            "Engine",
            "Class",
            "TexPanner",
            "Pan",
            "M",
        ];
        let n = |text: &str| names.iter().position(|&n| n == text).unwrap() as i32;
        let imports = [
            (n("Core"), n("Package"), 0, n("Engine")),
            (n("Core"), n("Class"), -1, n("TexPanner")),
        ];
        let mut data = compact_index(n("M"));
        data.push(0x22);
        data.extend(m.to_le_bytes());
        data.push(0);
        package_file(&names, &imports, &[(-2, 0, n("Pan"), 0x00070004, data)])
    };
    let dir = TempDir::new().unwrap();
    fs::create_dir(dir.path().join("old")).unwrap();
    fs::write(dir.path().join("old/MyPkg.u"), package(1)).unwrap();
    fs::write(dir.path().join("MyPkg.u"), package(2)).unwrap();
    pkg_diff(dir.path(), &["--exit-code"]).code(0).stdout("");
    pkg_diff(dir.path(), &["--all", "--format", "concise"])
        .code(0)
        .stdout("~ TexPanner'MyPkg.Pan' M=1 -> 2 (volatile)\n");
    let out = pkg_diff(dir.path(), &["--all", "--format", "json"])
        .get_output()
        .stdout
        .clone();
    let json: serde_json::Value = serde_json::from_slice(&out).unwrap();
    assert_eq!(
        json["exports"][0]["properties"][0]["volatile"],
        "set from the time by GetMatrix whenever it's used"
    );
    let out = pkg_diff(dir.path(), &["--format", "json"])
        .get_output()
        .stdout
        .clone();
    let json: serde_json::Value = serde_json::from_slice(&out).unwrap();
    assert_eq!(json["exports"], serde_json::json!([]));
}

/// A Git LFS pointer to an object of `size` bytes named `oid`.
fn lfs_pointer(oid: &str, size: usize) -> String {
    format!(
        "version https://git-lfs.github.com/spec/v1\noid sha256:{}\nsize {}\n",
        oid, size
    )
}

#[test]
fn pkg_diff_is_offline_unless_asked() {
    if std::process::Command::new("git")
        .arg("--version")
        .output()
        .is_err()
    {
        return;
    }
    // `MyPkg.u` committed as a Git LFS pointer, as Git LFS commits it, then
    // the new package in the working tree. The pointer's object isn't in the
    // cache yet.
    let oid = "ab12".to_string() + &"0".repeat(60);
    let old = diff_package(false);
    let dir = TempDir::new().unwrap();
    fs::write(dir.path().join("MyPkg.u"), lfs_pointer(&oid, old.len())).unwrap();
    git(dir.path(), &["init", "-q"]);
    git(dir.path(), &["add", "."]);
    git(dir.path(), &["commit", "-q", "-m", "old"]);
    fs::write(dir.path().join("MyPkg.u"), diff_package(true)).unwrap();
    let diff = |args: &[&str]| {
        usagi(dir.path())
            .args(["pkg", "diff", "MyPkg.u", "--color", "never"])
            .args(args)
            .assert()
    };
    let not_downloaded = "usagi: HEAD:MyPkg.u: it's in Git LFS, and its object isn't downloaded; \
                          nothing was downloaded, since `pkg diff` works offline unless \
                          `--online` is given\n";
    diff(&[]).code(2).stderr(not_downloaded);
    diff(&["--offline"]).code(2).stderr(not_downloaded);
    diff(&["--online", "--offline"])
        .code(2)
        .stderr(not_downloaded);
    // Online, through git's filters: with no Git LFS filter here, that's the
    // pointer itself.
    diff(&["--online"]).code(2).stderr(predicate::str::contains(
        "HEAD:MyPkg.u: not an Unreal package",
    ));
    // Only part of the object is in the cache.
    let objects = dir.path().join(".git/lfs/objects/ab/12");
    fs::create_dir_all(&objects).unwrap();
    fs::write(objects.join(&oid), &old[..10]).unwrap();
    diff(&[]).code(2).stderr(not_downloaded);
    // All of it.
    fs::write(objects.join(&oid), &old).unwrap();
    let expected = diff_text("HEAD:MyPkg.u", "MyPkg.u");
    diff(&[]).code(0).stdout(expected.clone());
    diff(&["--offline", "--online", "--offline"])
        .code(0)
        .stdout(expected.clone());
    // In a cache elsewhere (`lfs.storage`).
    git(dir.path(), &["config", "lfs.storage", "elsewhere"]);
    diff(&[]).code(2).stderr(not_downloaded);
    let elsewhere = dir.path().join(".git/elsewhere/objects/ab/12");
    fs::create_dir_all(&elsewhere).unwrap();
    fs::write(elsewhere.join(&oid), &old).unwrap();
    diff(&[]).code(0).stdout(expected);
    // A pointer with no size, or a short object name, isn't one usagi reads.
    fs::write(dir.path().join("Short.u"), lfs_pointer("ab", 4)).unwrap();
    fs::write(
        dir.path().join("Odd.u"),
        "version https://git-lfs.github.com/spec/v1\noid sha256:ab12\n",
    )
    .unwrap();
    git(dir.path(), &["add", "Short.u", "Odd.u"]);
    git(dir.path(), &["commit", "-q", "-m", "odd"]);
    usagi(dir.path())
        .args(["pkg", "diff", "HEAD", "HEAD", "Short.u"])
        .assert()
        .code(2)
        .stderr(predicate::str::contains("its object isn't downloaded"));
    usagi(dir.path())
        .args(["pkg", "diff", "HEAD", "HEAD", "Odd.u"])
        .assert()
        .code(2)
        .stderr(predicate::str::contains("Odd.u: not an Unreal package"));
}

#[test]
fn pkg_diff_a_git_lfs_pointer_in_the_working_tree() {
    let dir = TempDir::new().unwrap();
    fs::write(dir.path().join("Old.u"), diff_package(false)).unwrap();
    fs::write(dir.path().join("New.u"), lfs_pointer(&"0".repeat(64), 10)).unwrap();
    usagi(dir.path())
        .args(["pkg", "diff", "Old.u", "New.u"])
        .assert()
        .code(2)
        .stderr("usagi: New.u: it's a Git LFS pointer: its object isn't downloaded\n");
}
