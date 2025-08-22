from pathlib import Path
from typing import Optional

from pydantic import BaseModel, Field, FilePath

__all__ = ["PatchFileInputs"]


class PatchFileInputs(BaseModel):
    file: FilePath = Field(
        description="Pathname of input file, the file to be patched."
    )
    output_directory: Path = Field(
        description="Pathname to output directory where patches should be written."
    )
    patch_file: Path = Field(
        description="Pathname to which the patch file should be written [DEPRECATED].",
    )
    patched_file: Path = Field(
        description="If supplied, a pathname to which the patched file should be written."
    )
    patch_function: Optional[str] = Field(
        default=None, description="If known, the name of the function to patch."
    )
    line_number: Optional[int] = Field(
        default=None,
        description=(
            "If known, a line number that's likely to be flawed. "
            "E.g., a line number executed immediately before sanitizer trip"
        ),
    )
