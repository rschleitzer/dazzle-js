<?xml version="1.0"?>
<!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN" [
  <!ENTITY helpers SYSTEM "helpers.scm">
  <!ENTITY rules SYSTEM "rules-dsl.scm">
]>
<style-sheet>
<style-specification>
&helpers;
&rules;

;; Start processing
(process-root)
</style-specification>
</style-sheet>
