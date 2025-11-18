#!/bin/bash
# Test simple-page-sequence with margins: sps_css.dsl
# Flow objects: simple-page-sequence with left-margin, top-margin properties
# Output: FOT format
export SP_ENCODING=utf-8
openjade -t fot -d sps_css.dsl sps_css.sgml > sps_css.fot
