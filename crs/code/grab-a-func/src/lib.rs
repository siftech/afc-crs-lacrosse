mod path_rewrites;

pub use crate::path_rewrites::PathRewrites;
use anyhow::{anyhow, Context, Result};
use object::{elf, read::elf::ElfFile, LittleEndian, Object, ObjectSymbol};
use serde::Serialize;
use std::{fs, path::{Path, PathBuf}};
use tree_sitter::{Parser, Point};

pub type Elf<'a> = ElfFile<'a, elf::FileHeader64<LittleEndian>>;

#[derive(Debug, PartialEq, Serialize)]
pub struct SourceChunk {
    pub path: PathBuf,
    pub line: u32,
    pub column: u32,
    pub source: String,
}

/// Returns the source decl corresponding to an ELF symbol.
pub fn grab_a_symbol(
    path_rewrites: &PathRewrites,
    binary: &Path,
    symbol_name: &str,
) -> Result<SourceChunk> {
    // Parse the ELF file.
    let exe_bytes = fs::read(binary).unwrap();
    let elf = Elf::parse(&exe_bytes).unwrap();

    // Find the address for the symbol.
    //
    // TODO: Does this handle mangled names?
    let symbol = elf
        .symbol_by_name(symbol_name)
        .with_context(|| anyhow!("Failed to find the symbol {symbol_name:?} in {binary:?}"))?;

    // Get the source corresponding to the address.
    grab_an_address(path_rewrites, binary, symbol.address()).with_context(|| {
        anyhow!("Failed to find the source decl for the symbol {symbol_name:?} in {binary:?}")
    })
}

/// Returns the source decl corresponding to an address.
pub fn grab_an_address(path_rewrites: &PathRewrites, binary: &Path, addr: u64) -> Result<SourceChunk> {
    // Find the location for the address.
    let addr2line = addr2line::Loader::new(binary)
        .map_err(|err| anyhow!("Failed to load debug info from {binary:?}: {err}"))?;
    let location = addr2line
        .find_location(addr)
        .map_err(|err| {
            anyhow!(
                "Failed to find the source location of the address {addr:#x} in {binary:?}: {err}"
            )
        })?
        .with_context(|| {
            anyhow!("Failed to find the source location of the address {addr:#x} in {binary:?}")
        })?;

    // Extract the location's parts.
    //
    // TODO: Be more tolerant of bad locations. In particular:
    //
    // - If the file is missing, offer to search a whole directory recursively.
    // - If the line is missing, offer to search the whole file.
    // - If the column is missing, offer to search the whole line.
    let file = location.file.with_context(|| {
        anyhow!("Failed to find the source file for the address {addr:#x} in {binary:?}")
    })?;
    let line = location.line.with_context(|| {
        anyhow!("Failed to find the source line for the address {addr:#x} in {binary:?}")
    })?;
    let column = location.column.with_context(|| {
        anyhow!("Failed to find the source column for the address {addr:#x} in {binary:?}")
    })?;

    // Rewrite the file path according to the arguments.
    let rewritten_file = path_rewrites.rewrite(file.into());

    // Read the file.
    let src = fs::read_to_string(&rewritten_file).with_context(|| {
        anyhow!("Failed to read the source file {rewritten_file:?} (rewritten from {file:?}) to extract the source decl for the address {addr:#x} in {binary:?}")
    })?;

    // Get the source out of the file.
    let decl_src = grab_a_location(&src, line, column).with_context(|| {
        anyhow!("Failed to find the source decl for the address {addr:#x} in {binary:?} in the source file {rewritten_file:?} (rewritten from {file:?})")
    })?;
    Ok(SourceChunk {
        path: rewritten_file,
        line,
        column,
        source: decl_src.to_string(),
    })
}

/// Returns the source decl corresponding to a line and column inside source code.
pub fn grab_a_location(src: &str, line: u32, column: u32) -> Result<&str> {
    // Set up the parser.
    let mut parser = Parser::new();
    parser
        .set_language(&tree_sitter_c::language())
        .context("Failed to configure tree-sitter language")?;

    // Parse the source and get a cursor over it.
    let tree = parser.parse(src, None).context("Failed to parse file")?;
    let mut cursor = tree.walk();
    cursor.goto_first_child_for_point(Point {
        row: (line - 1) as usize,
        column: column as usize,
    });

    // Return the source range.
    Ok(&src[cursor.node().byte_range()])
}
