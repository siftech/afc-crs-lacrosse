use grab_a_func::{grab_a_symbol, PathRewrites};
use std::{path::Path, process::Command};
use tempdir::TempDir;

fn cc(src: &str, dst: impl AsRef<Path>) {
    let dst = dst.as_ref();
    let mut command = Command::new("cc");
    command.arg("-g").arg("-o").arg(dst).arg(src).arg("-lm");
    let status = command.status().unwrap();
    if !status.success() {
        panic!("Failed to run {command:?}: {status}");
    }
}

fn test_grab_a_symbol(exe: &Path, symbol: &str, line: u32, column: u32, source: &str) {
    let path_rewrites = PathRewrites::from_args(&[]).unwrap();
    let chunk = grab_a_symbol(&path_rewrites, &exe, symbol).unwrap();
    assert_eq!(
        (chunk.line, chunk.column, &chunk.source as &str),
        (line, column, source)
    );
}

#[test]
fn example_funcs() {
    let dir = TempDir::new("example_funcs").unwrap();
    let exe = dir.path().join("main");
    cc("tests/main.c", &exe);

    test_grab_a_symbol(
        &exe,
        "easy_example",
        3,
        32,
        r#"int easy_example(int a, int b) {
  b <<= 1;
  return a + b;
}"#,
    );

    test_grab_a_symbol(
        &exe,
        "with_a_macro",
        10,
        25,
        r#"int with_a_macro(int x) {
  const unsigned mask = (1 << MAGIC_NUMBER) - 1;
  return (x >> MAGIC_NUMBER) | (x & mask);
}"#,
    );

    test_grab_a_symbol(
        &exe,
        "like_a_syscall",
        17,
        26,
        r#"DEFUN(like_a_syscall, x) {
          x += 2;
          return x;
        }"#,
    );

    test_grab_a_symbol(
        &exe,
        "main",
        24,
        31,
        r#"int TERRIBLE_ADD_N(mai)(void) {
  int a = 2, b = 3;
  int c = easy_example(a, b);
  int d = with_a_macro(c);
  int e = like_a_syscall(d);
  return e;
}"#,
    );

    test_grab_a_symbol(
        &exe,
        "type_on_prev_lines_n",
        35,
        52,
        r#"struct {
  int x;
  int y;
} TERRIBLE_ADD_N(type_on_prev_lines_)(float theta) {
  float x = cos(theta);
  float y = cos(theta);
}"#,
    );
}
