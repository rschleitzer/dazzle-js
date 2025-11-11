//! SGML marked section preprocessing
//!
//! Expands SGML conditional marked sections before XML parsing.
//!
//! SGML marked sections allow conditional inclusion of content:
//! ```sgml
//! <!ENTITY % feature "INCLUDE">
//! <![%feature[ (content here) ]]>
//! ```
//!
//! LibXML2 (XML parser) doesn't understand these SGML constructs,
//! so we preprocess them before parsing.

use std::collections::HashMap;

/// Preprocess SGML marked sections in template content
///
/// This expands SGML conditional inclusion directives:
/// - Parse `<!ENTITY % name "INCLUDE|IGNORE">` declarations
/// - Expand `<![%name[...]]>` based on entity values
/// - INCLUDE: keep content, IGNORE: remove content
///
/// Returns the expanded content ready for XML parsing.
pub fn expand_marked_sections(content: &str) -> Result<String, String> {
    // Quick check: if no marked sections, return unchanged
    if !content.contains("<![%") {
        return Ok(content.to_string());
    }

    // Phase 1: Parse parameter entity declarations
    let entities = parse_parameter_entities(content);

    // Phase 2: Expand marked sections
    expand_conditional_sections(content, &entities)
}

/// Preprocess SGML marked sections with external parameter entities
///
/// This variant allows passing in parameter entities defined in other files.
/// Useful when entity declarations are split across multiple files (e.g.,
/// parameter entities in `.ent` files, marked sections in `.dsl` files).
///
/// The external_entities map is merged with any local parameter entity
/// declarations found in the content. Local declarations take precedence.
pub fn expand_marked_sections_with_entities(
    content: &str,
    external_entities: &HashMap<String, String>,
) -> Result<String, String> {
    // Quick check: if no marked sections, return unchanged
    if !content.contains("<![%") {
        return Ok(content.to_string());
    }

    // Phase 1: Parse parameter entity declarations from content
    let mut entities = external_entities.clone();

    // Local declarations override external ones
    let local_entities = parse_parameter_entities(content);
    entities.extend(local_entities);

    // Phase 2: Expand marked sections
    expand_conditional_sections(content, &entities)
}

/// Extract parameter entity declarations from content (public API)
///
/// Extracts declarations like:
/// `<!ENTITY % l10n-en "INCLUDE">`
///
/// This is useful when you need to collect parameter entity declarations
/// from multiple files before preprocessing them.
///
/// Returns a map of entity name → "INCLUDE" or "IGNORE"
pub fn extract_parameter_entities(content: &str) -> HashMap<String, String> {
    parse_parameter_entities(content)
}

/// Parse parameter entity declarations (internal)
///
/// Extracts declarations like:
/// `<!ENTITY % l10n-en "INCLUDE">`
///
/// Returns a map of entity name → "INCLUDE" or "IGNORE"
fn parse_parameter_entities(content: &str) -> HashMap<String, String> {
    let mut entities = HashMap::new();

    // Match: <!ENTITY % name "value">
    // Simple regex-free parsing for reliability
    let mut pos = 0;
    while let Some(start) = content[pos..].find("<!ENTITY") {
        let start = pos + start;
        if let Some(end) = content[start..].find('>') {
            let decl = &content[start..start + end + 1];

            // Parse the declaration
            if let Some((name, value)) = parse_entity_decl(decl) {
                entities.insert(name, value);
            }

            pos = start + end + 1;
        } else {
            break;
        }
    }

    entities
}

/// Parse a single entity declaration
fn parse_entity_decl(decl: &str) -> Option<(String, String)> {
    // Expected format: <!ENTITY % name "value">

    // Find the '%' that marks a parameter entity
    let percent_pos = decl.find('%')?;

    // Extract name (between % and first quote)
    let after_percent = &decl[percent_pos + 1..];
    let name_part = after_percent.split('"').next()?.trim();

    // Extract value (between quotes)
    let parts: Vec<&str> = decl.split('"').collect();
    if parts.len() >= 2 {
        let value = parts[1].trim();
        Some((name_part.to_string(), value.to_string()))
    } else {
        None
    }
}

/// Expand conditional marked sections
///
/// Processes `<![%entity[...]]>` based on entity values:
/// - If entity == "INCLUDE": replace with content
/// - If entity == "IGNORE": remove entirely
/// - If entity not found: keep as-is (conservative)
fn expand_conditional_sections(
    content: &str,
    entities: &HashMap<String, String>,
) -> Result<String, String> {
    let mut result = String::with_capacity(content.len());
    let mut pos = 0;

    while pos < content.len() {
        // Find next marked section
        if let Some(start) = content[pos..].find("<![%") {
            let start = pos + start;

            // Copy everything before the marked section
            result.push_str(&content[pos..start]);

            // Parse the marked section
            if let Some((entity_name, section_content, end)) = parse_marked_section(&content[start..]) {
                let end = start + end;

                // Check entity value
                match entities.get(&entity_name).map(|s| s.as_str()) {
                    Some("INCLUDE") => {
                        // Include the content
                        result.push_str(&section_content);

                        // Debug: show English INCLUDE expansions
                        if entity_name == "l10n-en" {
                            eprintln!("[SGML_DEBUG] INCLUDE entity=l10n-en, content_preview={:?}",
                                &section_content[..std::cmp::min(80, section_content.len())]);
                        }
                    }
                    Some("IGNORE") | None => {
                        // Skip the content (don't add anything)
                        // Per SGML/DSSSL semantics: undefined entities default to IGNORE
                    }
                    _ => {
                        // Other entity value (shouldn't happen): skip content
                        // Only INCLUDE and IGNORE are valid per SGML spec
                    }
                }

                pos = end;
            } else {
                // Malformed marked section: keep original and continue
                result.push_str(&content[start..start + 4]);
                pos = start + 4;
            }
        } else {
            // No more marked sections: copy rest of content
            result.push_str(&content[pos..]);
            break;
        }
    }

    Ok(result)
}

/// Parse a single marked section
///
/// Input: `<![%entity[ content ]]>`
/// Returns: (entity_name, content, total_length)
fn parse_marked_section(text: &str) -> Option<(String, String, usize)> {
    // Expected format: <![%name[ ... ]]>

    if !text.starts_with("<![%") {
        return None;
    }

    // Find the entity name (between % and [)
    let after_percent = &text[4..]; // Skip "<![%"
    let bracket_pos = after_percent.find('[')?;
    let entity_name_raw = &after_percent[..bracket_pos];
    // Strip trailing semicolon if present (SGML standard: <![%name;[ vs custom: <![%name[)
    let entity_name = entity_name_raw.trim().trim_end_matches(';').to_string();

    // Find the closing ]]>
    let content_start = 4 + bracket_pos + 1;
    let closing = text.find("]]>")?;

    if closing < content_start {
        return None;
    }

    let content = text[content_start..closing].to_string();
    let total_length = closing + 3; // Include ]]>

    Some((entity_name, content, total_length))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_entity_decl() {
        let decl = r#"<!ENTITY % l10n-en "INCLUDE">"#;
        let result = parse_entity_decl(decl);
        assert_eq!(result, Some(("l10n-en".to_string(), "INCLUDE".to_string())));

        let decl2 = r#"<!ENTITY % feature "IGNORE">"#;
        let result2 = parse_entity_decl(decl2);
        assert_eq!(result2, Some(("feature".to_string(), "IGNORE".to_string())));
    }

    #[test]
    fn test_parse_marked_section() {
        let text = r#"<![%l10n-en[ (("en") foo) ]]>"#;
        let result = parse_marked_section(text);
        assert_eq!(
            result,
            Some(("l10n-en".to_string(), " ((\"en\") foo) ".to_string(), 29))
        );
    }

    #[test]
    fn test_parse_marked_section_with_semicolon() {
        // SGML standard syntax includes semicolon before opening bracket
        let text = r#"<![%l10n-af;[<!ENTITY dbl1af "foo">]]>"#;
        let result = parse_marked_section(text);
        assert_eq!(
            result,
            Some(("l10n-af".to_string(), "<!ENTITY dbl1af \"foo\">".to_string(), 38))
        );
    }

    #[test]
    fn test_expand_include() {
        let content = r#"
<!ENTITY % feature "INCLUDE">
(define test
  <![%feature[ (included-code) ]]>
  (other-code))
"#;
        let result = expand_marked_sections(content).unwrap();
        assert!(result.contains("(included-code)"));
        assert!(!result.contains("<![%"));
    }

    #[test]
    fn test_expand_ignore() {
        let content = r#"
<!ENTITY % feature "IGNORE">
(define test
  <![%feature[ (ignored-code) ]]>
  (other-code))
"#;
        let result = expand_marked_sections(content).unwrap();
        assert!(!result.contains("(ignored-code)"));
        assert!(!result.contains("<![%"));
        assert!(result.contains("(other-code)"));
    }

    #[test]
    fn test_multiple_sections() {
        let content = r#"
<!ENTITY % en "INCLUDE">
<!ENTITY % de "IGNORE">
<![%en[ (english) ]]>
<![%de[ (deutsch) ]]>
"#;
        let result = expand_marked_sections(content).unwrap();
        assert!(result.contains("(english)"));
        assert!(!result.contains("(deutsch)"));
    }

    #[test]
    fn test_no_marked_sections() {
        let content = "(define test (code))";
        let result = expand_marked_sections(content).unwrap();
        assert_eq!(result, content);
    }

    #[test]
    fn test_case_clause_pattern() {
        // Test the actual pattern used in DocBook stylesheets
        let content = r#"
<!ENTITY % l10n-en "INCLUDE">
<!ENTITY % l10n-de "IGNORE">
(define (test-function lang)
  (case lang
    <![%l10n-en[ (("en") "English") ]]>
    <![%l10n-de[ (("de") "German") ]]>
    (else "Unknown")))
"#;
        let result = expand_marked_sections(content).unwrap();
        println!("==== RESULT ====");
        println!("{}", result);
        println!("================");
        assert!(result.contains(r#"(("en") "English")"#));
        assert!(!result.contains(r#"(("de") "German")"#));
        assert!(result.contains("(else"));
    }
}

