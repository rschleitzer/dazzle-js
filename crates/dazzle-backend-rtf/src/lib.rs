//! RTF backend for Dazzle document formatting
//!
//! This crate implements the `FotBuilder` trait for RTF (Rich Text Format) output,
//! which is used for document formatting and print output.
//!
//! ## Purpose
//!
//! The RTF backend generates formatted documents from DSSSL specifications,
//! supporting:
//!
//! - **Paragraphs**: Text blocks with formatting
//! - **Character properties**: Font, size, weight, posture, color
//! - **Page setup**: Margins, size, headers/footers
//! - **Document structure**: Sections, page sequences
//! - **Advanced features**: Links, tables, lists
//!
//! ## Architecture
//!
//! ```text
//! RtfBackend
//!   ├─ output: OutputByteStream (RTF file being written)
//!   ├─ font_table: FontTable (font declarations)
//!   ├─ color_table: ColorTable (color definitions)
//!   ├─ document_props: DocumentProperties (page setup, margins)
//!   └─ flow_stack: Vec<FlowObject> (nested flow object context)
//! ```
//!
//! ## RTF Format Overview
//!
//! RTF structure:
//! ```text
//! {\rtf1\ansi\deff0
//! {\fonttbl...}          # Font table
//! {\colortbl...}         # Color table
//! {\stylesheet...}       # Style definitions
//! \deflang1024           # Document properties
//! ...content...          # Paragraphs and text
//! }
//! ```
//!
//! ## Usage
//!
//! ```rust,ignore
//! use dazzle_backend_rtf::RtfBackend;
//! use dazzle_core::fot::FotBuilder;
//! use std::fs::File;
//!
//! let file = File::create("output.rtf")?;
//! let mut backend = RtfBackend::new(file)?;
//!
//! // Start a paragraph
//! backend.start_paragraph()?;
//! backend.literal("Hello, world!")?;
//! backend.end_paragraph()?;
//!
//! // Finalize and close
//! backend.finish()?;
//! ```

use dazzle_core::fot::FotBuilder;
use std::io::{Result, Write};

/// RTF backend for document formatting
///
/// Implements the `FotBuilder` trait with full support for DSSSL flow objects.
#[derive(Debug)]
pub struct RtfBackend<W: Write + std::fmt::Debug> {
    /// Output stream for RTF content
    output: W,

    /// Whether the document header has been written
    header_written: bool,

    /// Whether we're currently in a paragraph
    in_paragraph: bool,

    /// Current buffer for accumulating content before writing to file
    /// (used for entity flow object compatibility)
    current_buffer: String,
}

impl<W: Write + std::fmt::Debug> RtfBackend<W> {
    /// Get the inner writer by consuming self (for testing)
    #[cfg(test)]
    fn into_inner(self) -> W {
        use std::mem::ManuallyDrop;
        use std::ptr;

        // Prevent Drop from running
        let this = ManuallyDrop::new(self);

        // SAFETY: We're manually dropping and taking ownership of the field
        // This is safe because we're preventing Drop from running
        unsafe {
            ptr::read(&this.output)
        }
    }
    /// Create a new RTF backend
    ///
    /// # Arguments
    ///
    /// * `output` - Output stream to write RTF to
    ///
    /// # Example
    ///
    /// ```rust,ignore
    /// use std::fs::File;
    /// use dazzle_backend_rtf::RtfBackend;
    ///
    /// let file = File::create("output.rtf")?;
    /// let backend = RtfBackend::new(file)?;
    /// ```
    pub fn new(output: W) -> Result<Self> {
        Ok(RtfBackend {
            output,
            header_written: false,
            in_paragraph: false,
            current_buffer: String::new(),
        })
    }

    /// Write the RTF document header
    ///
    /// This includes:
    /// - RTF version and character set
    /// - Font table
    /// - Color table
    /// - Stylesheet
    /// - Default document properties
    fn write_header(&mut self) -> Result<()> {
        if self.header_written {
            return Ok(());
        }

        // RTF header with basic setup
        // Matches OpenJade's output structure
        writeln!(self.output, "{{\\rtf1\\ansi\\deff0")?;

        // Font table (basic fonts for now)
        writeln!(self.output, "{{\\fonttbl{{\\f1\\fnil\\fcharset0 Helvetica;}}")?;
        writeln!(self.output, "{{\\f6\\fnil\\fcharset161 Helvetica Greek;}}")?;
        writeln!(self.output, "{{\\f3\\fnil\\fcharset2 Symbol;}}")?;
        writeln!(self.output, "{{\\f2\\fnil\\fcharset0 Courier;}}")?;
        writeln!(self.output, "{{\\f5\\fnil\\fcharset161 Courier Greek;}}")?;
        writeln!(self.output, "{{\\f0\\fnil\\fcharset0 Times New Roman;}}")?;
        writeln!(self.output, "{{\\f4\\fnil\\fcharset161 Times New Roman Greek;}}")?;
        writeln!(self.output, "}}")?;

        // Color table (empty for now)
        writeln!(self.output, "{{\\colortbl;}}")?;

        // Stylesheet (basic styles)
        write!(self.output, "{{\\stylesheet")?;
        write!(self.output, "{{\\s1 Heading 1;}}")?;
        write!(self.output, "{{\\s2 Heading 2;}}")?;
        write!(self.output, "{{\\s3 Heading 3;}}")?;
        write!(self.output, "{{\\s4 Heading 4;}}")?;
        write!(self.output, "{{\\s5 Heading 5;}}")?;
        write!(self.output, "{{\\s6 Heading 6;}}")?;
        write!(self.output, "{{\\s7 Heading 7;}}")?;
        write!(self.output, "{{\\s8 Heading 8;}}")?;
        write!(self.output, "{{\\s9 Heading 9;}}")?;
        writeln!(self.output, "}}")?;

        // Document defaults
        writeln!(self.output, "\\deflang1024\\notabind\\facingp\\hyphauto1\\widowctrl")?;

        self.header_written = true;
        Ok(())
    }

    /// Finalize the RTF document
    ///
    /// Writes any pending content and closes the RTF structure.
    pub fn finish(&mut self) -> Result<()> {
        // Make sure header was written
        self.write_header()?;

        // Close any open paragraph
        if self.in_paragraph {
            writeln!(self.output, "\\par}}")?;
            self.in_paragraph = false;
        }

        // Close the RTF document
        writeln!(self.output, "}}")?;
        self.output.flush()?;

        Ok(())
    }
}

impl<W: Write + std::fmt::Debug> FotBuilder for RtfBackend<W> {
    // ============================================================================
    // Code Generation Primitives (for compatibility with SGML backend)
    // ============================================================================

    fn entity(&mut self, system_id: &str, _content: &str) -> Result<()> {
        // RTF backend doesn't support entity flow object (file writing)
        // This is used by SGML backend for code generation, not document formatting
        Err(std::io::Error::new(
            std::io::ErrorKind::Unsupported,
            format!("entity flow object not supported by RTF backend (attempted to write: {})", system_id),
        ))
    }

    fn formatting_instruction(&mut self, data: &str) -> Result<()> {
        // Append to buffer (used by SGML backend pattern)
        // For RTF, we buffer until a paragraph or other flow object uses it
        self.current_buffer.push_str(data);
        Ok(())
    }

    fn directory(&mut self, _path: &str) -> Result<()> {
        // RTF doesn't support directory creation
        Err(std::io::Error::new(
            std::io::ErrorKind::Unsupported,
            "directory flow object not supported by RTF backend",
        ))
    }

    fn current_output(&self) -> &str {
        &self.current_buffer
    }

    fn clear_buffer(&mut self) {
        self.current_buffer.clear();
    }

    // ============================================================================
    // Document Formatting Primitives (RTF-specific)
    // ============================================================================

    fn start_paragraph(&mut self) -> Result<()> {
        // Ensure header is written
        self.write_header()?;

        // Start a paragraph
        write!(self.output, "\\pard")?;
        self.in_paragraph = true;

        Ok(())
    }

    fn end_paragraph(&mut self) -> Result<()> {
        if !self.in_paragraph {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                "end_paragraph called without matching start_paragraph",
            ));
        }

        // End the paragraph with \par
        writeln!(self.output, "\\par")?;
        self.in_paragraph = false;

        Ok(())
    }

    fn literal(&mut self, text: &str) -> Result<()> {
        // Ensure we're in a paragraph
        if !self.in_paragraph {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                "literal text outside of paragraph",
            ));
        }

        // Write text, escaping RTF special characters
        for ch in text.chars() {
            match ch {
                '\\' => write!(self.output, "\\\\")?,
                '{' => write!(self.output, "\\{{")?,
                '}' => write!(self.output, "\\}}")?,
                '\n' => write!(self.output, "\\line ")?,
                '\t' => write!(self.output, "\\tab ")?,
                // Unicode characters > 127
                c if c as u32 > 127 => {
                    write!(self.output, "\\u{}?", c as u32)?;
                }
                c => write!(self.output, "{}", c)?,
            }
        }

        Ok(())
    }

    fn start_sequence(&mut self) -> Result<()> {
        // Sequence is just a container, no RTF output needed
        Ok(())
    }

    fn end_sequence(&mut self) -> Result<()> {
        // Sequence is just a container, no RTF output needed
        Ok(())
    }

    fn start_display_group(&mut self) -> Result<()> {
        // Display group - no special RTF output needed yet
        Ok(())
    }

    fn end_display_group(&mut self) -> Result<()> {
        // Display group - no special RTF output needed yet
        Ok(())
    }
}

impl<W: Write + std::fmt::Debug> Drop for RtfBackend<W> {
    fn drop(&mut self) {
        // Attempt to finalize on drop (best effort)
        let _ = self.finish();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Cursor;

    #[test]
    fn test_create_backend() {
        let cursor = Cursor::new(Vec::new());
        let backend = RtfBackend::new(cursor);
        assert!(backend.is_ok());
    }

    #[test]
    fn test_basic_paragraph() -> Result<()> {
        let cursor = Cursor::new(Vec::new());
        let mut backend = RtfBackend::new(cursor)?;

        backend.start_paragraph()?;
        backend.literal("Hello, world!")?;
        backend.end_paragraph()?;
        backend.finish()?;

        let output = String::from_utf8(backend.into_inner().into_inner()).unwrap();

        // Check that RTF header is present
        assert!(output.contains("{\\rtf1\\ansi\\deff0"));

        // Check that paragraph is present
        assert!(output.contains("\\pard"));
        assert!(output.contains("Hello, world!"));
        assert!(output.contains("\\par"));

        Ok(())
    }

    #[test]
    fn test_character_escaping() -> Result<()> {
        let cursor = Cursor::new(Vec::new());
        let mut backend = RtfBackend::new(cursor)?;

        backend.start_paragraph()?;
        backend.literal("Test { } \\ special")?;
        backend.end_paragraph()?;
        backend.finish()?;

        let output = String::from_utf8(backend.into_inner().into_inner()).unwrap();

        // Check that special characters are escaped
        assert!(output.contains("\\{"));
        assert!(output.contains("\\}"));
        assert!(output.contains("\\\\"));

        Ok(())
    }
}
