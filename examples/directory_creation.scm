;; Example: Using the directory flow object with functional, nested pattern
;;
;; Directories are container flow objects that nest naturally, just like entity:
;; - directory contains entities and other directories
;; - paths are resolved relative to their parent directory
;; - no procedural state management needed!
;;
;; Usage: dazzle -d examples/directory_creation.scm examples/sample.xml

(make directory path: "generated"
  (make directory path: "models"
    (make entity system-id: "User.rs"
      (make formatting-instruction data: "// Auto-generated User model\n")
      (make formatting-instruction data: "pub struct User {\n")
      (make formatting-instruction data: "    pub id: u32,\n")
      (make formatting-instruction data: "    pub name: String,\n")
      (make formatting-instruction data: "}\n"))
    (make entity system-id: "Post.rs"
      (make formatting-instruction data: "// Auto-generated Post model\n")
      (make formatting-instruction data: "pub struct Post {\n")
      (make formatting-instruction data: "    pub id: u32,\n")
      (make formatting-instruction data: "    pub title: String,\n")
      (make formatting-instruction data: "}\n")))
  (make directory path: "controllers"
    (make entity system-id: "UserController.rs"
      (make formatting-instruction data: "// Auto-generated User controller\n")
      (make formatting-instruction data: "pub struct UserController;\n")))
  (make directory path: "views"
    (make entity system-id: "user_view.html"
      (make formatting-instruction data: "<!-- Auto-generated User view -->\n")
      (make formatting-instruction data: "<div class=\"user\">\n")
      (make formatting-instruction data: "  <h1>User Profile</h1>\n")
      (make formatting-instruction data: "</div>\n"))))
