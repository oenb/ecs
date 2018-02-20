*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 07.12.2017 at 10:04:01
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZECS_CONFIG.....................................*
DATA:  BEGIN OF STATUS_ZECS_CONFIG                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZECS_CONFIG                   .
CONTROLS: TCTRL_ZECS_CONFIG
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZECS_CONFIG                   .
TABLES: ZECS_CONFIG                    .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
