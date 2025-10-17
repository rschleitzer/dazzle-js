//! Steel Scheme interpreter integration
//!
//! Sets up Steel Scheme engine and registers all DSSSL primitives

use anyhow::{Context, Result};
use steel::steel_vm::engine::Engine;
use steel::steel_vm::register_fn::RegisterFn;
use std::collections::HashMap;

use crate::primitives;

/// Scheme engine for template evaluation
pub struct SchemeEngine {
    pub(crate) engine: Engine,
    variables: HashMap<String, String>,
    search_paths: Vec<std::path::PathBuf>,
}

/// Prelude code that defines helper functions
const PRELUDE: &str = r#"
;; ============================================================================
;; DSSSL Compatibility Stubs
;; ============================================================================

;; define-language: DSSSL feature for character case mappings
;; Steel Scheme has built-in char-upcase and char-downcase
(define-syntax define-language
  (syntax-rules ()
    ((define-language name . rest) #f)))

;; declare-default-language: DSSSL feature
(define-syntax declare-default-language
  (syntax-rules ()
    ((declare-default-language name) #f)))

;; declare-flow-object-class: DSSSL metadata
(define-syntax declare-flow-object-class
  (syntax-rules ()
    ((declare-flow-object-class name public-id) #f)))

;; declare-characteristic: DSSSL metadata
(define-syntax declare-characteristic
  (syntax-rules ()
    ((declare-characteristic name public-id default-value) #f)))

;; external-procedure: DSSSL feature for calling external code
(define-syntax external-procedure
  (syntax-rules ()
    ((external-procedure public-id)
     (lambda args #f))))

;; ============================================================================
;; Global Variables
;; ============================================================================

;; Internal storage for grove and nodes (with mutation)
;; These are replaced when set_current_grove() is called from Rust
(define *current-grove-internal* #f)
(define *current-root-internal* #f)
(define *current-node-internal* #f)

;; DSSSL functions: current-node and current-root are FUNCTIONS, not variables!
(define (current-grove)
  *current-grove-internal*)

(define (current-root)
  *current-root-internal*)

(define (current-node)
  *current-node-internal*)

;; ============================================================================
;; DSSSL Compatibility Wrappers
;; ============================================================================

;; OpenJade-compatible element-with-id: takes just an ID, searches current grove
;; The Rust function is grove-element-with-id(grove, id)
(define (element-with-id id)
  (let ((grove (current-grove)))
    (if grove
        (grove-element-with-id grove id)
        #f)))

;; Grove query functions with optional node parameter (defaults to current-node)

(define gi
  (case-lambda
    (()
     (let ((node (current-node)))
       (if node (grove-gi-impl node) "")))
    ((node)
     (if node (grove-gi-impl node) ""))))

(define id
  (case-lambda
    (()
     (let ((val (grove-id-impl (current-node))))
       (if val val "")))
    ((node)
     (let ((val (grove-id-impl node)))
       (if val val "")))))

(define data
  (case-lambda
    (() (grove-data-impl (current-node)))
    ((node) (grove-data-impl node))))

;; attribute-string with optional node and default parameters
;; DSSSL: (attribute-string "name") uses current-node
;; DSSSL: (attribute-string "name" node) uses given node
;; DSSSL: (attribute-string "name" node "default") returns default if not found
;; Returns #f if attribute doesn't exist (DSSSL standard)
(define attribute-string
  (case-lambda
    ((name) (grove-attribute-string-impl name (current-node)))
    ((name node) (grove-attribute-string-impl name node))
    ((name node default)
      (let ((val (grove-attribute-string-impl name node)))
        (if val val default)))))

;; Variadic sosofo-append (wrapper around binary sosofo-append-two)
(define (sosofo-append . sosofos)
  (cond
    ((null? sosofos) (empty-sosofo))
    ((null? (cdr sosofos)) (car sosofos))
    (else (sosofo-append-two (car sosofos) (apply sosofo-append (cdr sosofos))))))

;; Variadic node-list constructor (wrapper around node-list-from-vec)
;; Usage: (node-list node1 node2 node3) or (apply node-list '(node1 node2))
(define (node-list . nodes)
  (node-list-from-vec nodes))

;; ============================================================================
;; Node-list operations with predicates
;; ============================================================================

(define (node-list-filter pred nl)
  "Filter node-list by predicate function"
  (define (filter-helper nodes acc)
    (if (node-list-empty? nodes)
        acc
        (let ((first (node-list-first nodes))
              (rest (node-list-rest nodes)))
          (if (pred first)
              (filter-helper rest (cons first acc))
              (filter-helper rest acc)))))
  (let ((result (filter-helper nl '())))
    (if (null? result)
        (empty-node-list)
        (apply node-list (reverse result)))))

(define (node-list-some pred nl)
  "Test if any node in node-list matches predicate"
  (define (some-helper nodes)
    (if (node-list-empty? nodes)
        #f
        (let ((first (node-list-first nodes)))
          (if (pred first)
              #t
              (some-helper (node-list-rest nodes))))))
  (some-helper nl))

(define (node-list-map proc nl)
  "Map procedure over node-list"
  (define (map-helper nodes acc)
    (if (node-list-empty? nodes)
        (reverse acc)
        (let ((first (node-list-first nodes))
              (rest (node-list-rest nodes)))
          (map-helper rest (cons (proc first) acc)))))
  (map-helper nl '()))

(define (node-list-count nl)
  "Count nodes in node-list"
  (node-list-length nl))

(define (node-list-contains? nl node)
  "Check if node-list contains a specific node"
  (node-list-some (lambda (n) (equal? n node)) nl))

(define (node-list-last nl)
  "Get the last node in a node-list"
  (if (node-list-empty? nl)
      nl  ; Return empty node-list
      (let ((rest (node-list-rest nl)))
        (if (node-list-empty? rest)
            (node-list-first nl)
            (node-list-last rest)))))

(define (node-list=? nl1 nl2)
  "Check if two node-lists are equal"
  (cond
    ((and (node-list-empty? nl1) (node-list-empty? nl2)) #t)
    ((or (node-list-empty? nl1) (node-list-empty? nl2)) #f)
    (else (and (equal? (node-list-first nl1) (node-list-first nl2))
               (node-list=? (node-list-rest nl1) (node-list-rest nl2))))))

;; ============================================================================
;; DSSSL Construction Rules System with MODES
;; ============================================================================

;; Mode registry: nested association lists
;; Structure: ((mode-symbol . ((gi-symbol . procedure) ...)) ...)
;; Default mode is symbol 'default-mode
(define *modes* '((default-mode . ())))

;; Default rules (per mode): ((mode-symbol . procedure) ...)
(define *default-rules* '())

;; Track which mode we're DEFINING rules for
(define *defining-mode* 'default-mode)

;; Track which mode we're PROCESSING with
(define *processing-mode* 'default-mode)

;; ============================================================================
;; Helper Functions for Mode Management
;; ============================================================================

;; Get rules alist for a mode
(define (get-mode-rules mode-symbol)
  (let ((entry (assq mode-symbol *modes*)))
    (if entry
        (cdr entry)
        '())))

;; Set rules alist for a mode (functional - rebuilds list)
(define (set-mode-rules! mode-symbol rules-alist)
  (define (update-alist alist)
    (cond
      ((null? alist) (list (cons mode-symbol rules-alist)))
      ((eq? (caar alist) mode-symbol) (cons (cons mode-symbol rules-alist) (cdr alist)))
      (else (cons (car alist) (update-alist (cdr alist))))))
  (set! *modes* (update-alist *modes*)))

;; Register element rule in specific mode
(define (register-element-rule mode-symbol gi-symbol proc)
  (let ((rules (get-mode-rules mode-symbol)))
    (set-mode-rules! mode-symbol (cons (cons gi-symbol proc) rules))))

;; Register default rule for specific mode (functional - rebuilds list)
(define (register-default-rule mode-symbol proc)
  (define (update-alist alist)
    (cond
      ((null? alist) (list (cons mode-symbol proc)))
      ((eq? (caar alist) mode-symbol) (cons (cons mode-symbol proc) (cdr alist)))
      (else (cons (car alist) (update-alist (cdr alist))))))
  (set! *default-rules* (update-alist *default-rules*)))

;; ============================================================================
;; TRUE DSSSL MACROS
;; ============================================================================

;; element macro: (element gi body...)
;; Registers rule in current *defining-mode*
(define-syntax element
  (syntax-rules ()
    ((element gi body ...)
     (register-element-rule *defining-mode*
                            (quote gi)
                            (lambda () body ...)))))

;; default macro: (default body...)
;; Registers default rule in current *defining-mode*
(define-syntax default
  (syntax-rules ()
    ((default body ...)
     (register-default-rule *defining-mode*
                             (lambda () body ...)))))

;; mode macro: (mode name rule-definition...)
;; Temporarily sets *defining-mode* while defining rules
(define-syntax mode
  (syntax-rules ()
    ((mode mode-name body ...)
     (let ((saved-defining-mode *defining-mode*))
       (set! *defining-mode* (quote mode-name))
       body ...
       (set! *defining-mode* saved-defining-mode)))))

;; with-mode macro: (with-mode name body...)
;; Temporarily sets *processing-mode* while executing body
(define-syntax with-mode
  (syntax-rules ()
    ((with-mode mode-name body ...)
     (let ((saved-processing-mode *processing-mode*))
       (set! *processing-mode* (quote mode-name))
       (let ((result (begin body ...)))
         (set! *processing-mode* saved-processing-mode)
         result)))))

;; make macro: (make flow-class characteristics... content...)
;; Simplified for code generation - characteristics are keyword: value pairs
(define-syntax make
  (syntax-rules (sequence entity system-id: formatting-instruction data:)
    ;; (make sequence content...) - just concatenate sosofos
    ((make sequence content ...)
     (sosofo-append content ...))

    ;; (make entity system-id: "file.txt" content...)
    ((make entity system-id: filename content ...)
     (make-entity filename (sosofo-append content ...)))

    ;; (make formatting-instruction data: content)
    ((make formatting-instruction data: content)
     (make-formatting-instruction content))

    ;; Fallback: treat as sequence
    ((make flow-class content ...)
     (sosofo-append content ...))))

;; ============================================================================
;; Processing Functions (Mode-Aware)
;; ============================================================================

;; Find rule for a given GI in current processing mode
(define (find-rule gi-symbol)
  "Look up construction rule by GI symbol in current processing mode"
  (let ((rules (get-mode-rules *processing-mode*)))
    ;; Use assoc instead of assq - Steel doesn't intern symbols from string->symbol
    (let ((entry (assoc gi-symbol rules)))
      (if entry
          (cdr entry)
          #f))))

;; Find default rule for current processing mode
(define (find-default-rule)
  "Look up default rule for current processing mode"
  (let ((entry (assq *processing-mode* *default-rules*)))
    (if entry
        (cdr entry)
        #f)))

;; Process a single node by applying its construction rule
(define (process-node node)
  "Apply construction rule to a node (mode-aware)"
  (if (and node (element? node))
      (let ((gi-symbol (string->symbol (gi node)))
            (saved-node (current-node)))
        ;; Rebind current-node for this rule's execution
        (set! *current-node-internal* node)
        (let ((rule (find-rule gi-symbol)))
          (let ((result (cond
                          ;; Element has specific rule in current mode
                          ((procedure? rule) (rule))
                          ;; Use default rule for current mode if defined
                          ((find-default-rule) => (lambda (default) (default)))
                          ;; No rule: return empty
                          (else (empty-sosofo)))))
            ;; Restore previous current-node
            (set! *current-node-internal* saved-node)
            result)))
      ;; Text nodes: return empty (or could return their content)
      (empty-sosofo)))

;; Process all children of current-node
(define (process-children)
  "Process children of current-node using construction rules"
  (let ((child-nodes (children (current-node))))
    (process-node-list child-nodes)))

;; Process a node-list (optimized for performance)
(define (process-node-list nl)
  "Process each node in node-list and combine results"
  (if (node-list-empty? nl)
      (empty-sosofo)
      (sosofo-append
        (process-node (node-list-first nl))
        (process-node-list (node-list-rest nl)))))

;; Process starting from root
(define (process-root)
  "Start processing from current-root"
  (let ((result (process-node (current-root))))
    ;; Auto-write entity flow objects
    (write-sosofo result)
    result))

;; ============================================================================
;; R5RS multi-list map (Steel's map only supports single lists)
;; This MUST be at the end, after all uses of map in the prelude
;; ============================================================================

;; Save Steel's built-in single-list map before redefining
(define steel-builtin-map map)

;; Helper: check if any list in a list-of-lists is empty
(define (any-null? lists)
  (if (null? lists)
      #f
      (if (null? (car lists))
          #t
          (any-null? (cdr lists)))))

;; R5RS-compliant map that supports multiple lists
(set! map
  (lambda (proc . lists)
    "R5RS map supporting multiple lists"
    (cond
      ;; Single list: use Steel's built-in map
      ((= (length lists) 1)
       (steel-builtin-map proc (car lists)))

      ;; Multiple lists: implement multi-list mapping
      ((> (length lists) 1)
       (let loop ((lsts lists))
         ;; Check if any list is empty
         (if (any-null? lsts)
             '()
             ;; Apply proc to first element of each list, then recurse on rest
             (cons (apply proc (steel-builtin-map car lsts))
                   (loop (steel-builtin-map cdr lsts))))))

      ;; Edge case: no lists
      (else '()))))
"#;

impl SchemeEngine {
    /// Create a new Scheme engine with DSSSL primitives registered
    pub fn new() -> Result<Self> {
        let engine = Engine::new();

        let mut scheme_engine = Self {
            engine,
            variables: HashMap::new(),
            search_paths: vec![std::env::current_dir()?],  // Default: current directory
        };

        // Register all DSSSL primitives
        primitives::grove::register_grove_primitives(&mut scheme_engine)?;
        primitives::processing::register_processing_primitives(&mut scheme_engine)?;
        primitives::util::register_util_primitives(&mut scheme_engine)?;

        // Load prelude (defines *current-grove-internal*, etc. and helper functions)
        scheme_engine.engine
            .compile_and_run_raw_program(PRELUDE.to_string())
            .map_err(|e| anyhow::anyhow!("Failed to load prelude: {:?}", e))?;

        Ok(scheme_engine)
    }

    /// Register a Rust function as a Scheme primitive
    ///
    /// This delegates to Steel's Engine::register_fn
    pub fn register_fn<FN, ARGS, RET>(&mut self, name: &'static str, func: FN) -> &mut Self
    where
        Engine: RegisterFn<FN, ARGS, RET>,
    {
        self.engine.register_fn(name, func);
        self
    }

    /// Set a variable (from CLI -V flags)
    pub fn set_variable(&mut self, name: String, value: String) {
        self.variables.insert(name.clone(), value.clone());

        // Also inject into Scheme environment as a global
        let _ = self.engine.compile_and_run_raw_program(format!(
            "(define {} \"{}\")",
            name,
            value.replace("\"", "\\\"")
        ));
    }

    /// Get a variable value
    pub fn get_variable(&self, name: &str) -> Option<&str> {
        self.variables.get(name).map(|s| s.as_str())
    }

    /// Add a template search directory
    pub fn add_search_path(&mut self, path: std::path::PathBuf) {
        if !self.search_paths.contains(&path) {
            self.search_paths.push(path);
        }
    }

    /// Get search paths (for debugging)
    pub fn search_paths(&self) -> &[std::path::PathBuf] {
        &self.search_paths
    }

    /// Find a file in the search paths
    fn find_in_search_paths(&self, filename: &str) -> Option<std::path::PathBuf> {
        use std::path::Path;

        // If it's an absolute path or contains path separators, use it directly
        let path = Path::new(filename);
        if path.is_absolute() || filename.contains('/') || filename.contains('\\') {
            if path.exists() {
                return Some(path.to_path_buf());
            }
            return None;
        }

        // Search in each search path
        for search_dir in &self.search_paths {
            let candidate = search_dir.join(filename);
            if candidate.exists() {
                return Some(candidate);
            }
        }

        None
    }

    /// Load and evaluate a Scheme file or XML-embedded DSSSL
    pub fn load_file(&mut self, path: &str) -> Result<()> {
        // Try to find the file in search paths
        let file_path = self.find_in_search_paths(path)
            .ok_or_else(|| anyhow::anyhow!("File not found in search paths: {}", path))?;

        let contents = std::fs::read_to_string(&file_path)
            .with_context(|| format!("Failed to read file: {}", file_path.display()))?;

        // Detect if this is XML or pure Scheme
        let scheme_code = if contents.trim_start().starts_with("<") {
            // XML-embedded DSSSL - extract from <style-specification>
            self.extract_dsssl_from_xml(&file_path)?
        } else {
            // Pure Scheme file
            contents
        };

        self.engine
            .compile_and_run_raw_program(scheme_code)
            .map_err(|e| anyhow::anyhow!("Scheme error in {}: {:?}", file_path.display(), e))?;

        Ok(())
    }

    /// Extract DSSSL code from XML style-sheet
    /// Handles entity expansion automatically via libxml2
    fn extract_dsssl_from_xml(&self, file_path: &std::path::Path) -> Result<String> {
        use libxml::bindings::{xmlReadFile, xmlParserOption_XML_PARSE_NOENT, xmlParserOption_XML_PARSE_DTDLOAD};
        use libxml::tree::Document;
        use libxml::xpath::Context;
        use std::ffi::CString;

        // Parse XML with entity substitution using xmlReadFile directly
        let path_cstr = CString::new(file_path.to_str().unwrap())
            .map_err(|_| anyhow::anyhow!("Invalid path"))?;

        let doc_ptr = unsafe {
            xmlReadFile(
                path_cstr.as_ptr(),
                std::ptr::null(),
                (xmlParserOption_XML_PARSE_NOENT | xmlParserOption_XML_PARSE_DTDLOAD) as i32,  // Enable entity substitution and DTD loading
            )
        };

        if doc_ptr.is_null() {
            anyhow::bail!("Failed to parse XML template: {}", file_path.display());
        }

        // Wrap in Document for safe handling (Document's Drop will free the doc)
        let doc = Document::new_ptr(doc_ptr);

        // Use XPath to find <style-specification> or <STYLE-SPECIFICATION> element (case-insensitive)
        let context = Context::new(&doc)
            .map_err(|_| anyhow::anyhow!("Failed to create XPath context"))?;

        // Try lowercase first
        let mut result = context
            .evaluate("//style-specification")
            .map_err(|_| anyhow::anyhow!("XPath evaluation failed"))?;

        let mut nodes = result.get_nodes_as_vec();

        // If not found, try uppercase (OpenJade style)
        if nodes.is_empty() {
            result = context
                .evaluate("//STYLE-SPECIFICATION")
                .map_err(|_| anyhow::anyhow!("XPath evaluation failed"))?;
            nodes = result.get_nodes_as_vec();
        }

        if nodes.is_empty() {
            anyhow::bail!("No <style-specification> or <STYLE-SPECIFICATION> element found in XML template");
        }

        // Extract text content (entities now expanded by libxml2)
        let node = &nodes[0];
        let text = node.get_content();

        // Document's Drop will clean up automatically

        Ok(text)
    }

    /// Evaluate a Scheme expression and return the result as a string
    pub fn eval(&mut self, expr: &str) -> Result<String> {
        let results = self
            .engine
            .compile_and_run_raw_program(expr.to_string())
            .map_err(|e| anyhow::anyhow!("Scheme error: {:?}", e))?;

        // Return the last result (like a REPL)
        if let Some(last) = results.last() {
            Ok(format!("{}", last))
        } else {
            Ok(String::from("#<void>"))
        }
    }

    /// Get direct access to the Steel engine for advanced operations
    pub fn engine_mut(&mut self) -> &mut Engine {
        &mut self.engine
    }

    /// Set the current grove for template processing
    /// This makes the grove available to the template via `current-grove`
    /// and the root node available via `current-root`
    pub fn set_current_grove(&mut self, grove: crate::grove::Grove) -> Result<()> {
        use steel::gc::Gc;
        use steel::rvals::SteelVal;

        // We need to keep the Grove (which owns the Document) alive
        // So we'll register both the grove and provide a function to get the root

        // Get the root node
        let root = grove.root().clone();

        // Wrap values in SteelVal and register them with unique names
        let grove_val = SteelVal::Custom(Gc::new_mut(Box::new(grove)));
        let root_val = SteelVal::Custom(Gc::new_mut(Box::new(root.clone())));
        let node_val = SteelVal::Custom(Gc::new_mut(Box::new(root)));

        // Register with temporary names (register_value creates NEW bindings)
        self.engine.register_value("***grove-tmp***", grove_val);
        self.engine.register_value("***root-tmp***", root_val);
        self.engine.register_value("***node-tmp***", node_val);

        // Now use set! to update the internal variables defined in the prelude
        // Also redefine the accessor functions to ensure they capture the current scope
        let code = r#"
(set! *current-grove-internal* ***grove-tmp***)
(set! *current-root-internal* ***root-tmp***)
(set! *current-node-internal* ***node-tmp***)

(set! current-grove (lambda () *current-grove-internal*))
(set! current-root (lambda () *current-root-internal*))
(set! current-node (lambda () *current-node-internal*))
"#;
        self.eval(code)?;

        Ok(())
    }
}

impl Default for SchemeEngine {
    fn default() -> Self {
        Self::new().expect("Failed to create SchemeEngine")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_engine_creation() {
        let _engine = SchemeEngine::new().unwrap();
    }

    #[test]
    fn test_variables() {
        let mut engine = SchemeEngine::new().unwrap();
        engine.set_variable("foo".to_string(), "bar".to_string());
        assert_eq!(engine.get_variable("foo"), Some("bar"));
    }

    #[test]
    fn test_eval_arithmetic() {
        let mut engine = SchemeEngine::new().unwrap();
        let result = engine.eval("(+ 2 3)").unwrap();
        assert_eq!(result, "5");
    }

    #[test]
    fn test_eval_string() {
        let mut engine = SchemeEngine::new().unwrap();
        let result = engine.eval(r#"(string-append "Hello" " " "World")"#).unwrap();
        assert!(result.contains("Hello World"));
    }

    #[test]
    fn test_grove_primitive_available() {
        let mut engine = SchemeEngine::new().unwrap();
        // Test that our registered primitives are available
        // gi, children, etc. should be defined
        let result = engine.eval("(procedure? gi)");
        assert!(result.is_ok());
    }
}
