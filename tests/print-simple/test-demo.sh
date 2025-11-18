#!/bin/bash
# Test the simplest print example: demo.dsl
# Flow objects: simple-page-sequence, scroll, paragraph, text
# Output: FOT format
export SP_ENCODING=utf-8
openjade -t fot -d demo.dsl demo.sgm
