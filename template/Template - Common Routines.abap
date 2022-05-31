INCLUDE zag_utils.
*&---------------------------------------------------------------------*
* - 00 - INDEX
*&---------------------------------------------------------------------*

* - 01 - RETURN FORMATTED MESSAGES FROM SYSTEM
* - 02 - RETURN FORMATTED MESSAGES FROM CUST
* - 03 - RETURN NEXT NUMBER FROM NUMERIC RANGE SNRO
* - 04 - FILL INTERNAL RANGE FROM SET GS01/GS02/GS03
* - 05 - CUSTOM MATCHCODE IN SELECTION SCREEN
* - 06 - ALM BUFFER REFRESH
* - 07 - CHANGE MANTAINANCE ORDER STATUS
* - 08 - MATCHCODE FOR INPUT FILEPATH
* - 09 - MATCHCODE FOR OUTPUT FILEPATH
* - 10 - READ DATA FROM FILE
* - 11 - WRITE DATA ON FILE
* - 12 - DYNAMIC READING OF A STRUCTURE
* - 13 - CONVERT DATA TO INTERNAL
* - 14 - CONVERT DATA TO EXTERNAL
* - 15 - BUILD HEADER FROM DDIC
* - 16 - DISPLAY GENERIC ALV IN POPUP
* - 17 - SEND MAIL
* - 18 - REMOVE SPECIAL CHARACTERS
* - 19 - BUILD HEADER FROM ITAB
* - 20 - GET DESKTOP DIRECTORY
* - 21 - GET TRANSPOSED TABLE


*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&------------------------ IMPLEMENTATION -----------------------------*
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
* - 01 - RETURN FORMATTED MESSAGES FROM SYSTEM
*&---------------------------------------------------------------------*
FORM format_syst_msg CHANGING y_msg TYPE bapi_msg.

*  DATA: lv_msg TYPE bapi_msg.
*  PERFORM format_syst_msg CHANGING  lv_msg.

  CALL FUNCTION 'FORMAT_MESSAGE'
    EXPORTING
      id        = sy-msgid
      lang      = '-D'
      no        = sy-msgno
      v1        = sy-msgv1
      v2        = sy-msgv2
      v3        = sy-msgv3
      v4        = sy-msgv4
    IMPORTING
      msg       = y_msg
    EXCEPTIONS
      not_found = 1
      OTHERS    = 2.
  IF sy-subrc <> 0.
  ENDIF.

ENDFORM.                    " FORMAT_RETURN
*&---------------------------------------------------------------------*
* - 02 - RETURN FORMATTED MESSAGES FROM CUST
*&---------------------------------------------------------------------*
FORM format_cust_msg  USING    x_id     TYPE sy-msgid
                               x_number TYPE sy-msgno
                               x_msgv1  TYPE sy-msgv1
                               x_msgv2  TYPE sy-msgv2
                               x_msgv3  TYPE sy-msgv3
                               x_msgv4  TYPE sy-msgv4
                      CHANGING y_msg    TYPE string.

*  DATA: lv_msg TYPE string.
*  CONSTANTS: c_my_msg TYPE bapiret2-id VALUE 'ZMY_MSG'.
*  PERFORM format_cust_msg USING     c_my_msg '001' 'VAR1' 'VAR2' 'VAR3' 'VAR4'
*                          CHANGING   lv_msg.

  CALL FUNCTION 'FORMAT_MESSAGE'
    EXPORTING
      id        = x_id
      lang      = '-D'
      no        = x_number
      v1        = x_msgv1
      v2        = x_msgv2
      v3        = x_msgv3
      v4        = x_msgv4
    IMPORTING
      msg       = y_msg
    EXCEPTIONS
      not_found = 1
      OTHERS    = 2.
  IF sy-subrc <> 0.
  ENDIF.

ENDFORM.                    " FORMAT_RETURN
*&---------------------------------------------------------------------*
* - 03 - RETURN NEXT NUMBER FROM NUMERIC RANGE SNRO
*&---------------------------------------------------------------------*
FORM number_get_next USING    xs_inri       TYPE inri
                     CHANGING y_next_number
                              y_msg         TYPE string.


*  DATA: ls_inri        TYPE inri,
*        lv_next_number TYPE c LENGTH 10,
*        lv_msg         TYPE bapi_msg.
*
*  CONSTANTS: c_range_name TYPE nrobj VALUE 'ZRANGE_NAME'.
*
*  ls_inri-object    = c_range_name.
*  ls_inri-nrrangenr = '01'.
*  ls_inri-toyear    = sy-datum(4).
*  ls_inri-quantity  = '1'.

*  PERFORM number_get_next USING    ls_inri
*                          CHANGING lv_next_number
*                                   lv_msg.

  CLEAR: y_next_number, y_msg.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = xs_inri-nrrangenr
      object                  = xs_inri-object
      quantity                = xs_inri-quantity
      toyear                  = xs_inri-toyear
    IMPORTING
      number                  = y_next_number
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.
  IF sy-subrc <> 0.

*    PERFORM format_syst_msg CHANGING y_msg.

  ENDIF.

ENDFORM.                    " NUMBER_GET_NEXT
*&---------------------------------------------------------------------*
* - 04 - FILL INTERNAL RANGE FROM SET GS01/GS02/GS03
*&---------------------------------------------------------------------*
FORM get_values_from_set  USING    x_setname       TYPE string
                          CHANGING yt_range_values TYPE tt_hrrange.

  "TODO dichiarare tt_hrrange nel top
*  TYPES: tt_hrrange TYPE TABLE OF hrrange.
*  DATA: lr_my_range TYPE tt_hrrange.
*  CONSTANTS: c_my_set_name TYPE string VALUE 'ZMY_SET_NAME'.

*  PERFORM get_values_from_set USING    c_my_set_name
*                              CHANGING lr_my_range.

  DATA: lv_setid    TYPE sethier-setid,
        lt_values   TYPE STANDARD TABLE OF rgsbv,
        lv_setname  TYPE c LENGTH 24.

  REFRESH yt_range_values[].

  lv_setname = x_setname.
  CONDENSE lv_setname NO-GAPS.

  CLEAR lv_setid.
  CALL FUNCTION 'G_SET_GET_ID_FROM_NAME'
    EXPORTING
      shortname = lv_setname       "Set Name
    IMPORTING
      new_setid = lv_setid
    EXCEPTIONS
      OTHERS    = 1.

  IF sy-subrc EQ 0.

    REFRESH lt_values.
    CALL FUNCTION 'G_SET_FETCH'
      EXPORTING
        setnr           = lv_setid
      TABLES
        set_lines_basic = lt_values
      EXCEPTIONS
        OTHERS          = 1.

    CHECK lt_values[] IS NOT INITIAL.

    yt_range_values = value #(
              FOR <value> in lt_values
           ( SIGN = 'I' opti = 'EQ'
           LOW = <value>-from  HIGH = <value>-to ) ).

  ENDIF.

ENDFORM.          " GET_VALUES_FROM_SET
*&---------------------------------------------------------------------*
* - 05 - CUSTOM MATCHCODE IN SELECTION SCREEN
*&---------------------------------------------------------------------*
FORM f4_my_field .

  "TODO Change the name based on field needed
  "Adapt SFLIGHT table to your own
*  PARAMETERS: p_carrid TYPE sflight-carrid,
*              p_fldate TYPE sflight-fldate.
*
*  PERFORM f4_my_field.

  DATA: lt_datatab      TYPE STANDARD TABLE OF datatab,
        lt_return_tab   TYPE TABLE OF ddshretval.

  FIELD-SYMBOLS: <datatab>   LIKE LINE OF lt_datatab,
                 <return>    LIKE LINE OF lt_return_tab.

  REFRESH lt_datatab[].
  SELECT *
    FROM datatab
    INTO TABLE lt_datatab
    WHERE myfield EQ p_carrid.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'MYFIELD'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
*      dynprofield     = 'P_FLDATE' "Fill only if you want fill SEL SCREEN
      value_org       = 'S'
    TABLES
      return_tab      = lt_return_tab
      value_tab       = lt_datatab
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  READ TABLE lt_return_tab ASSIGNING <return> INDEX 1.
  IF sy-subrc EQ 0.
    p_fldate = <return>-fieldval.
  ENDIF.

  CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
    EXPORTING
      functioncode           = '=ENT'
    EXCEPTIONS
      function_not_supported = 1
      OTHERS                 = 2.

ENDFORM.                    " F4_MY_FIELD
*&---------------------------------------------------------------------*
* - 06 - ALM BUFFER REFRESH
*&---------------------------------------------------------------------*
FORM alm_buffer_refresh .

*  PERFORM alm_buffer_refresh.

  CALL FUNCTION 'ISU_ORDER_RESET_CREATE_STATUS'.

  CALL FUNCTION 'IM_SM_DATA_RESET'.

  CALL FUNCTION 'CO_ZF_DATA_RESET_COMPLETE'.

  CALL FUNCTION 'IBAPI_Z_SET_BAPI_FLAG'
    EXPORTING
      iv_flag = space.

ENDFORM.          " ALM_BUFFER_REFRESH
*&---------------------------------------------------------------------*
* - 07 - CHANGE MANTAINANCE ORDER STATUS
*&---------------------------------------------------------------------*
FORM status_change_extern USING x_aufnr   TYPE aufnr
                x_my_status TYPE j_txt04.

*  DATA: lv_aufnr      TYPE aufk-aufnr,
*        lv_my_status  TYPE tj30t-txt04.
*
*  lv_my_status = 'ZMYS'.
*
*  PERFORM status_change_extern USING lv_aufnr
*                                     lv_my_status.

  DATA: lv_objnr       TYPE aufk-objnr,
        lv_stsma       TYPE jsto-stsma,
        lv_user_status TYPE jest-stat.

  CLEAR lv_objnr.
  SELECT SINGLE objnr
    FROM aufk
    INTO lv_objnr
    WHERE aufnr EQ x_aufnr.

  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

  CALL FUNCTION 'STATUS_TEXT_EDIT'
    EXPORTING
      client           = sy-mandt
      flg_user_stat    = 'X'
      objnr            = lv_objnr
      only_active      = 'X'
      spras            = sy-langu
    IMPORTING
      e_stsma          = lv_stsma
    EXCEPTIONS
      object_not_found = 1
      OTHERS           = 2.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

  CLEAR lv_user_status.
  SELECT SINGLE estat
    FROM tj30t
    INTO lv_user_status
    WHERE stsma EQ lv_stsma
      AND spras EQ sy-langu
      AND txt04 EQ x_my_status.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

  CALL FUNCTION 'STATUS_CHANGE_EXTERN'
    EXPORTING
      client              = sy-mandt
      objnr               = lv_objnr
      user_status         = lv_user_status
    EXCEPTIONS
      object_not_found    = 1
      status_inconsistent = 2
      status_not_allowed  = 3
      OTHERS              = 4.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

ENDFORM.                    " STATUS_CHANGE_EXTERN
*&---------------------------------------------------------------------*
* - 08 - MATCHCODE FOR INPUT FILEPATH
*&---------------------------------------------------------------------*
FORM help_file_input CHANGING y_path_input.

*  DATA p_serv_x.
*  DATA p_locl_x.
*  PERFORM help_file CHANGING p_path.

  DATA: lv_path TYPE string.

  IF p_serv_x EQ 'X'.

    CALL FUNCTION '/SAPDMC/LSM_F4_SERVER_FILE'
*      EXPORTING
*       directory        = cv_pathup
*        filemask         = '.csv'
      IMPORTING
        serverfile       = lv_path
      EXCEPTIONS
        canceled_by_user = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
*     Implement suitable error handling here
    ELSE.
      y_path_input = lv_path.
    ENDIF.

  ELSEIF p_locl_x EQ 'X'.

    CALL FUNCTION 'WS_FILENAME_GET'
      EXPORTING
*        mask             = ',*.csv,*.csv.'
        mode             = 'O'
      IMPORTING
        filename         = lv_path
      EXCEPTIONS
        inv_winsys       = 01
        no_batch         = 02
        selection_cancel = 03
        selection_error  = 04.
    IF sy-subrc <> 0.
*     Implement suitable error handling here
    ELSE.
      y_path_input = lv_path.
    ENDIF.

  ENDIF.

ENDFORM.                    " HELP_FILE_INPUT


*&---------------------------------------------------------------------*
* - 09 - MATCHCODE FOR OUTPUT FILEPATH
*&---------------------------------------------------------------------*
FORM help_file_output CHANGING y_path_output.

*  DATA p_serv_y.
*  DATA p_locl_y.
*  PERFORM help_file_o CHANGING p_path_o.

  DATA: lv_path TYPE string.

  IF p_serv_y EQ 'X'.

    CALL FUNCTION '/SAPDMC/LSM_F4_SERVER_FILE'
      EXPORTING
*        directory        = ''
        filemask         = '.hello'
      IMPORTING
        serverfile       = lv_path
      EXCEPTIONS
        canceled_by_user = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
*     Implement suitable error handling here
    ELSE.
      y_path_output = lv_path.
    ENDIF.

  ELSEIF p_locl_y EQ 'X'.

    CALL METHOD cl_gui_frontend_services=>directory_browse
      EXPORTING
        window_title         = ''
        initial_folder       = ''
      CHANGING
        selected_folder      = lv_path
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4.
    IF sy-subrc <> 0.
      "implement suitable error handling here
    ELSE.
      CALL METHOD cl_gui_cfw=>flush( ).
      y_path_output = lv_path.
    ENDIF.
  ENDIF.
  
ENDFORM.                    " HELP_FILE_OUTPUT
*&---------------------------------------------------------------------*
* - 10 - READ DATA FROM FILE
*&---------------------------------------------------------------------*
  
  TYPES: BEGIN OF ty_csv_input.
        INCLUDE TYPE zyout_Structure.
  TYPES: icon   TYPE icon_d,
         msg    TYPE bapi_msg.
  TYPES: END OF ty_csv_input.

  DATA: gt_csv_input TYPE TABLE OF ty_csv_input.
 
  CLEAR: gt_csv_input[].

  IF rb_loc EQ 'X'.
    PERFORM upload_locale.

  ELSEIF rb_srv EQ 'X'.
    PERFORM upload_server.

  ENDIF.


*--------------------------------------------------------------------*

 FORM upload_locale .

  DATA: lv_string_path TYPE string,
        lt_row_data    TYPE STANDARD TABLE OF string,
        lv_header_read  .

  FIELD-SYMBOLS: <row_data>  LIKE LINE OF lt_row_data,
                 <csv_input> LIKE LINE OF gt_csv_input.

*--------------------------------------------------------------------*

  lv_string_path = p_path.
  lv_header_read = space.

  IF lv_string_path CS '.xlsx'.

    PERFORM upload_excel TABLES lt_row_data
                         USING '.xlsx'.

  ELSEIF lv_string_path CS '.xls'.

    PERFORM upload_excel TABLES lt_row_data
                         USING '.xls'.

  ELSEIF lv_string_path CS '.csv'.

    CLEAR lt_row_data[].
    CALL METHOD cl_gui_frontend_services=>gui_upload
      EXPORTING
        filename = lv_string_path
        filetype = 'ASC'
      CHANGING
        data_tab = lt_row_data[]
      EXCEPTIONS
        OTHERS   = 1.
    IF sy-subrc <> 0.
      MESSAGE s000(db) WITH text-e11 DISPLAY LIKE 'E'.
      STOP.
    ENDIF.

  ENDIF.

  UNASSIGN <row_data>.
  LOOP AT lt_row_data ASSIGNING <row_data>.

    IF p_header EQ 'X'.
      IF lv_header_read EQ space.
        lv_header_read = 'X'.
        CONTINUE.
      ENDIF.
    ENDIF.

    UNASSIGN <csv_input>.
    APPEND INITIAL LINE TO gt_csv_input ASSIGNING <csv_input>.

    PERFORM convert_row_to_sap USING    <row_data>
                               CHANGING <csv_input>.

  ENDLOOP.

  IF gt_csv_input[] IS INITIAL.
    MESSAGE s000(db) WITH text-e12 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

ENDFORM.                    " UPLOAD_LOCALE

*--------------------------------------------------------------------*

FORM upload_excel  TABLES yt_row_data
                   USING  x_format    TYPE string.

  DATA: lv_filename    TYPE rlgrap-filename,
        lt_intern      TYPE TABLE OF alsmex_tabline,
        lt_intern_indx TYPE TABLE OF alsmex_tabline,
        ls_intern      LIKE LINE OF lt_intern,
        lv_end_col     TYPE i,
        lv_end_row     TYPE i,
        lt_row_data    TYPE TABLE OF string,
        lv_tabix       TYPE n LENGTH 4,
        lv_new_val     TYPE string.

  DATA: lo_structdescr    TYPE REF TO cl_abap_structdescr.

  FIELD-SYMBOLS: <intern>     LIKE LINE OF lt_intern,
                 <intern_tmp> LIKE LINE OF lt_intern,
                 <row_data>   LIKE LINE OF lt_row_data..

  FIELD-SYMBOLS: <sap_line>   TYPE ANY,
                 <component>  LIKE LINE OF lo_structdescr->components,
                 <dyn_value>  TYPE ANY.

*--------------------------------------------------------------------*
  lv_filename = p_path.

  CASE x_format.
    WHEN '.xlsx'.
      lv_end_row = 1048576.
      lv_end_col = 16384 .

    WHEN '.xls'.
      lv_end_row = 65536.
      lv_end_col = 256.

    WHEN OTHERS.
      EXIT.
  ENDCASE.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = lv_filename
      i_begin_col             = 1
      i_begin_row             = 1
      i_end_col               = lv_end_col
      i_end_row               = lv_end_row
    TABLES
      intern                  = lt_intern
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  lo_structdescr ?= cl_abap_typedescr=>describe_by_name( c_structure_csv ).
  CHECK lo_structdescr IS NOT INITIAL.

  SORT lt_intern BY row col.

  lt_intern_indx[] = lt_intern[].
  DELETE ADJACENT DUPLICATES FROM lt_intern_indx COMPARING row.

  CLEAR: yt_row_data[], lt_row_data[].
  LOOP AT lt_intern_indx ASSIGNING <intern>.

    UNASSIGN <row_data>.
    APPEND INITIAL LINE TO lt_row_data ASSIGNING <row_data>.

    LOOP AT lo_structdescr->components ASSIGNING <component>.
      lv_tabix = sy-tabix.

      UNASSIGN <intern_tmp>.
      READ TABLE lt_intern ASSIGNING <intern_tmp>
        WITH KEY row = <intern>-row
                 col = lv_tabix
                 BINARY SEARCH.
      IF sy-subrc EQ 0.
        lv_new_val = <intern_tmp>-value.

      ELSE.
        lv_new_val = '$'.

      ENDIF.

      IF <row_data> IS INITIAL.
        <row_data> = lv_new_val.
      ELSE.
        CONCATENATE <row_data>
                    lv_new_val
                    INTO <row_data> SEPARATED BY ';'.
      ENDIF.

      REPLACE ALL OCCURRENCES OF '$' IN <row_data> WITH space.
    ENDLOOP.

  ENDLOOP.

  yt_row_data[] = lt_row_data[].

ENDFORM.                    " UPLOAD_EXCEL

*--------------------------------------------------------------------*

FORM upload_server .

  DATA: lv_string_path TYPE string,
        lv_row_data    TYPE string,
        lv_header_read.

  FIELD-SYMBOLS: <csv_input> LIKE LINE OF gt_csv_input.

*--------------------------------------------------------------------*

  lv_string_path = p_path.

  IF lv_string_path CS '.xlsx'
    OR lv_string_path CS '.xls'.

    MESSAGE s000(db) WITH text-e13 text-e14 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  lv_header_read = space.

  OPEN DATASET lv_string_path FOR INPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc <> 0.
    CLOSE DATASET lv_string_path.
    MESSAGE s000(db) WITH text-e11 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  DO.

    CLEAR lv_row_data.
    READ DATASET lv_string_path INTO lv_row_data.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

    IF p_header EQ 'X'.
      IF lv_header_read EQ space.
        lv_header_read = 'X'.
        CONTINUE.
      ENDIF.
    ENDIF.

    UNASSIGN <csv_input>.
    APPEND INITIAL LINE TO gt_csv_input ASSIGNING <csv_input>.

    PERFORM convert_row_to_sap USING    lv_row_data
                               CHANGING <csv_input>.

  ENDDO.

  CLOSE DATASET lv_string_path.

  IF gt_csv_input[] IS INITIAL.
    MESSAGE s000(db) WITH text-e12 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

ENDFORM.                   

*&---------------------------------------------------------------------*
* - 11 - WRITE DATA ON FILE
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
* - 12 - DYNAMIC READING OF A STRUCTURE
*&---------------------------------------------------------------------*
FORM check_dyn_structure USING x_data
                               x_structure_name TYPE tabname.

*  "Adapt SFLIGHT table to your own
*  DATA: ls_sflight TYPE sflight.
*  CONSTANTS: c_my_struct_name TYPE tabname VALUE 'SFLIGHT'.
*  PERFORM check_dyn_structure USING ls_sflight
*                                    c_my_struct_name.

  DATA: lo_structdescr TYPE REF TO cl_abap_structdescr.

  FIELD-SYMBOLS: <component> LIKE LINE OF lo_structdescr->components,
                 <dyn_value> TYPE any.

  lo_structdescr ?= cl_abap_typedescr=>describe_by_name( x_structure_name ).
  CHECK lo_structdescr IS NOT INITIAL.

  UNASSIGN <component>.
  LOOP AT lo_structdescr->components ASSIGNING <component>.

    UNASSIGN <dyn_value>.
    ASSIGN COMPONENT <component>-name OF STRUCTURE x_data TO <dyn_value>.
    CHECK <dyn_value> IS ASSIGNED.

    "TODO Do something

  ENDLOOP.

ENDFORM.          " CHECK_DYN_STRUCTURE
*&---------------------------------------------------------------------*
* - 13 - CONVERT DATA TO INTERNAL
*&---------------------------------------------------------------------*
FORM convert_data_to_internal USING    value(x_data_ext)
                              CHANGING y_data_int TYPE dats.

*  DATA: lv_data_int TYPE dats,
*        lv_data_ext TYPE string.

*  PERFORM convert_data_to_internal USING    lv_data_ext
*                                   CHANGING lv_data_int.

  CHECK strlen( x_data_ext ) EQ 10.

  CONCATENATE x_data_ext+6(4)
              x_data_ext+3(2)
              x_data_ext(2)
         INTO y_data_int.


ENDFORM.          " CONVERT_DATA_TO_INTERNAL
*&---------------------------------------------------------------------*
* - 14 - CONVERT DATA TO EXTERNAL
*&---------------------------------------------------------------------*
FORM convert_data_to_external USING    x_data_int    TYPE dats
                                       x_separator
                              CHANGING value(y_data_ext).

*  DATA: lv_data_int TYPE dats,
*        lv_data_ext TYPE string,
*        lv_separator.
*
*  lv_separator = '/'.
*  PERFORM convert_data_to_external USING    lv_data_int
*                                            lv_separator
*                                   CHANGING lv_data_ext.

  CHECK x_data_int NE '00000000'.

  CONCATENATE x_data_int+6(2)
              x_data_int+4(2)
              x_data_int(4)
              INTO y_data_ext
              SEPARATED BY x_separator.

  CONDENSE y_data_ext NO-GAPS.


ENDFORM.          " CONVERT_DATA_TO_EXTERNAL
*&---------------------------------------------------------------------*
* - 15 - BUILD HEADER FROM DDIC
*&---------------------------------------------------------------------*
FORM get_header_output  USING    x_tracciato TYPE tabname
                        CHANGING y_header    TYPE string.

*  DATA: lv_nome_tracciato TYPE tabname,
*        lv_header         TYPE string.
*
*  PERFORM get_header_output USING   lv_nome_tracciato
*                            CHANGING  lv_header.

  DATA: lv_tabname      TYPE ddobjname,
        lv_fieldname    TYPE dfies-fieldname,
        lt_dfies_tab    TYPE STANDARD TABLE OF dfies,
        ls_dfies        LIKE LINE OF lt_dfies_tab,
        lv_new_col      TYPE string.

  DATA: lo_structdescr    TYPE REF TO cl_abap_structdescr.

  FIELD-SYMBOLS: <sap_line>  TYPE any,
                 <component> LIKE LINE OF lo_structdescr->components,
                 <dyn_value> TYPE any,
                 <difies>    LIKE LINE OF lt_dfies_tab.

*--------------------------------------------------------------------*

  CLEAR y_header.

  lo_structdescr ?= cl_abap_typedescr=>describe_by_name( x_tracciato ).
  CHECK lo_structdescr IS NOT INITIAL.

*--------------------------------------------------------------------*

  UNASSIGN <component>.
  LOOP AT lo_structdescr->components ASSIGNING <component>.

    lv_tabname   = x_tracciato.
    lv_fieldname = <component>-name.

    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname        = lv_tabname
        fieldname      = lv_fieldname
      TABLES
        dfies_tab      = lt_dfies_tab
      EXCEPTIONS
        not_found      = 1
        internal_error = 2
        OTHERS         = 3.

    CLEAR ls_dfies.
    READ TABLE lt_dfies_tab INTO ls_dfies INDEX 1.

    CLEAR lv_new_col.

    lv_new_col = ls_dfies-fieldtext.


    IF y_header IS INITIAL.
      y_header = lv_new_col.

    ELSE.
      CONCATENATE y_header
            lv_new_col
            INTO y_header
            SEPARATED BY ';'.

    ENDIF.

  ENDLOOP.

ENDFORM.                    " GET_HEADER_OUTPUT
*&---------------------------------------------------------------------*
* - 16 - DISPLAY GENERIC ALV IN POPUP
*&---------------------------------------------------------------------*
FORM display_alv_popup  TABLES xt_alv  TYPE STANDARD TABLE
                        USING  x_title TYPE lvc_title.

*   PERFORM display_alv_popup TABLES gt_sflight
*                              USING  'My Title'.

  DATA go_alv TYPE REF TO cl_salv_table.

  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = go_alv
        CHANGING
          t_table      = xt_alv[] ).

    CATCH cx_salv_msg.
  ENDTRY.

  DATA lr_columns TYPE REF TO cl_salv_columns_table.
  lr_columns = go_alv->get_columns( ).
  lr_columns->set_optimize( 'X' ). "--> Optimise all columns
  
  DATA lr_column TYPE REF TO cl_salv_column_table.
  TRY.
      lr_column ?= lr_columns->get_column( 'MANDT' ).
      lr_column->set_visible( value  = if_salv_c_bool_sap=>false ).
    CATCH cx_salv_not_found.
  ENDTRY.

  DATA: lr_functions TYPE REF TO cl_salv_functions_list.
  lr_functions = go_alv->get_functions( ).
  lr_functions->set_all( 'X' ).
  
  DATA: lr_display   TYPE REF TO cl_salv_display_settings.
  lr_display = go_alv->get_display_settings( ).
  lr_display->set_striped_pattern( cl_salv_display_settings=>true ).
  lr_display->set_list_header( x_title ).

  IF go_alv IS BOUND.

    go_alv->set_screen_popup(
      start_column = 1
      end_column  = 100
      start_line  = 1
      end_line    = 15 ).

  ENDIF.
  
  go_alv->display( ).

ENDFORM. "DISPLY_ALV_POPUP
*&---------------------------------------------------------------------*
*& - 17 - SEND MAIL
*&---------------------------------------------------------------------*
FORM send_mail .

  "Adapt the points marked with TODO tag
*  PERFORM send_mail.

  DATA: doc_chng LIKE sodocchgi1,
        objcont  LIKE TABLE OF solisti1 WITH HEADER LINE,
        content1 LIKE solisti1,
        entries  TYPE int4,
        reclist  LIKE TABLE OF somlreci1 WITH HEADER LINE,
        pack_wa TYPE sopcklsti1,
        packing_list TYPE TABLE OF sopcklsti1 INITIAL SIZE 1.

  DATA: header  TYPE thead,
        lines   TYPE TABLE OF tline,
        endline TYPE sy-tabix.

  "TODO adapt
  DATA: std_text_name TYPE thead-tdname VALUE 'ZYOUR_STD_TEXT'.
*--------------------------------------------------------------------*

  CLEAR: content1, objcont[], reclist[].

  CLEAR: header, lines[].
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id                      = 'ST'
      language                = sy-langu
      name                    = std_text_name
      object                  = 'TEXT'
    IMPORTING
      header                  = header
    TABLES
      lines                   = lines
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.
  IF sy-subrc EQ 0.

*    DESCRIBE TABLE lines LINES data(lv_lines).
    DATA lv_lines TYPE i.
    DESCRIBE TABLE lines LINES lv_lines.

*    LOOP AT lines ASSIGNING field-symbol(<lines>).
    FIELD-SYMBOLS: <lines> LIKE LINE OF lines.
    LOOP AT lines ASSIGNING <lines>.

      "TODO adapt
      IF <lines>-tdline CS '&your_variable&'.
        REPLACE '&your_variable&' IN <lines>-tdline WITH ''.
      ENDIF.

      content1 = <lines>-tdline.
      APPEND content1 TO objcont.
    ENDLOOP.

  ELSE.

    CONCATENATE ''
                'Corpo della mail'
                INTO content1
                SEPARATED BY space.

    APPEND content1 TO objcont.

  ENDIF.

  CLEAR entries.
  DESCRIBE TABLE objcont LINES entries.
  READ TABLE objcont INDEX entries.
  doc_chng-doc_size = ( entries - 1 ) * 255 + strlen( objcont ).

  "TODO adapt
  doc_chng-obj_name   = 'NOME_MAIL'.
  "TODO adapt
  doc_chng-obj_descr  = 'Oggetto della mail'.
  doc_chng-sensitivty = 'P'.

  DATA: lt_receiver TYPE TABLE OF tvarvc.
  SELECT * FROM tvarvc
    INTO TABLE lt_receiver
    WHERE name EQ 'ZYOUR_PARAM'. "TODO adapt

*  LOOP AT lt_receiver ASSIGNING field-symbol(<receiver>).
  FIELD-SYMBOLS: <receiver> LIKE LINE OF lt_receiver.
  LOOP AT lt_receiver ASSIGNING <receiver>.

    reclist-receiver = <receiver>-low.
    reclist-rec_type = 'U'.
    reclist-express  = 'X'.
    APPEND reclist.
  ENDLOOP.

  IF reclist[] IS INITIAL.
    "TODO adapt
    MESSAGE i001(00) WITH 'Destinatari mancanti in tabella TVARVC.'.
    EXIT.
  ENDIF.

  pack_wa-doc_type = 'HTM'.
  pack_wa-head_start = 1.
  pack_wa-head_num   = 0.
  pack_wa-body_start = 1.
  pack_wa-body_num = lines( objcont ).

  APPEND pack_wa TO packing_list.

  CALL FUNCTION 'SO_DOCUMENT_SEND_API1'
    EXPORTING
      document_data              = doc_chng
      sender_address             = 'YOUR_SENDED' "TODO adapt
      commit_work                = 'X'
    TABLES
      packing_list               = packing_list
      contents_txt               = objcont
      receivers                  = reclist
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.

ENDFORM.                    " SEND_MAIL
*&---------------------------------------------------------------------*
*& - 18 - REMOVE SPECIAL CHARACTERS
*&---------------------------------------------------------------------*
FORM remove_special_characters  CHANGING y_string TYPE text255.

*  DATA lv_string TYPE text255.
*  PERFORM remove_special_characters CHANGING lv_string.
  
  "Example from FM ES_REMOVE_SPECIAL_CHARACTER
  
  CONSTANTS: c_regex_lect_upper TYPE c LENGTH 255 VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ',
             c_regex_lect_lower TYPE c LENGTH 255 VALUE 'abcdefghijklmnopqrstuvwxyz',
             c_regex_digit      TYPE c LENGTH 255 VALUE '0123456789',
             c_regex_symb       TYPE c LENGTH 255 VALUE '!"%/=?;,.:-_@&+*()[]{}<>',
             c_regex_lect_acc   TYPE c LENGTH 255 VALUE 'èéàáòóùúÉÈÁÀÓÒÚÙ'.

  DATA(lv_length) = strlen( y_string ).

  DO lv_length TIMES.
    DATA(lv_index) = sy-index - 1.

    IF   y_string+lv_index(1) CA c_regex_lect_upper
      OR y_string+lv_index(1) CA c_regex_lect_lower
      OR y_string+lv_index(1) CA c_regex_digit
      OR y_string+lv_index(1) CA c_regex_symb
      OR y_string+lv_index(1) CA c_regex_lect_acc  .

      CONTINUE.

    ELSE.

      y_string+lv_index(1) = ''.

    ENDIF.

  ENDDO.

ENDFORM.

*&---------------------------------------------------------------------*
*& - 19 - BUILD HEADER FROM ITAB
*&---------------------------------------------------------------------*
FORM get_header CHANGING y_header TYPE string.

*  DATA: lv_header     TYPE string,
*  PERFORM get_header CHANGING lv_header.

  DATA: table TYPE REF TO data.

  y_header = ''.

  CREATE DATA table LIKE yourtab.
  ASSIGN table->* TO FIELD-SYMBOL(<table>).
  TRY.
      cl_salv_table=>factory( IMPORTING
                                r_salv_table   = DATA(salv_table)
                              CHANGING
                                t_table        = <table>  ).
      DATA(lt_fcat) = cl_salv_controller_metadata=>get_lvc_fieldcatalog(
          r_columns      = salv_table->get_columns( ) " ALV Filter
          r_aggregations = salv_table->get_aggregations( ) " ALV Aggregations
  ).
    CATCH cx_root.
  ENDTRY.


  LOOP AT lt_fcat ASSIGNING FIELD-SYMBOL(<fcat>).

    IF y_header IS INITIAL.
      y_header = <fcat>-reptext.

    ELSE.
      CONCATENATE y_header
                  <fcat>-reptext
             INTO y_header
             SEPARATED BY ';'.

    ENDIF.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& - 20 - GET DESKTOP DIRECTORY
*&---------------------------------------------------------------------*
FORM get_desktop_directory  CHANGING y_desktop_directory TYPE string.

*  DATA: lv_desktop_directory TYPE string.
*  PERFORM get_desktop_directory CHANGING lv_desktop_directory.

  y_desktop_directory = ''.
  CALL METHOD cl_gui_frontend_services=>get_desktop_directory
    CHANGING
      desktop_directory = y_desktop_directory
    EXCEPTIONS
      cntl_error        = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  CALL METHOD cl_gui_cfw=>update_view.

ENDFORM.                    " GET_DESKTOP_DIRECTORY

*&---------------------------------------------------------------------*
*& - 21 - GET TRANSPOSED TABLE
*&---------------------------------------------------------------------*
FORM get_transposed_table  TABLES   xt_table
                           CHANGING yp_transp      TYPE REF TO data
                                    yt_fcat_transp TYPE lvc_t_fcat.

  CLEAR: yp_transp, yt_fcat_transp[].

  CHECK lines( xt_table ) EQ 1.

  "------------------------------------------------
  "-> Creazione tabella DESCR_CAMPO + VALORE
  "-> con relativo riferimento <fs>

  DATA: lt_fcat   TYPE lvc_t_fcat,
        lp_transp TYPE REF TO data.

  FIELD-SYMBOLS: <fcat>           TYPE lvc_s_fcat,
                 <lt_transp_data> TYPE table.


  APPEND INITIAL LINE TO lt_fcat ASSIGNING <fcat>.
  <fcat>-fieldname  = 'COLUMNTEXT'.
  <fcat>-ref_table  = 'LVC_S_DETA'.

  APPEND INITIAL LINE TO lt_fcat ASSIGNING <fcat>.
  <fcat>-fieldname  = 'VALUE'.
  <fcat>-ref_field  = 'VALUE'.
  <fcat>-ref_table  = 'LVC_S_DETA'.

  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      it_fieldcatalog = lt_fcat
    IMPORTING
      ep_table        = lp_transp.

  ASSIGN lp_transp->* TO <lt_transp_data>.


  "------------------------------------------------
  "-> Estrazione lista campi tabella originale

  DATA: lo_alv        TYPE REF TO cl_salv_table,
        lr_columns    TYPE REF TO cl_salv_columns_table,
        lt_column_ref TYPE salv_t_column_ref.

  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = lo_alv
        CHANGING
          t_table      = xt_table[] ).

    CATCH cx_salv_msg.
  ENDTRY.

  lr_columns    = lo_alv->get_columns( ).
  lt_column_ref = lr_columns->get( ).


  "------------------------------------------------
  "-> Trasposizione campi da NxM a MxN

  READ TABLE xt_table ASSIGNING FIELD-SYMBOL(<original_row>) INDEX 1.

  LOOP AT lt_column_ref ASSIGNING FIELD-SYMBOL(<column_ref>).
  
    CHECK <column_ref>-columnname NE 'MANDT'.

    ASSIGN COMPONENT <column_ref>-columnname OF STRUCTURE <original_row> TO FIELD-SYMBOL(<value>).
    CHECK sy-subrc EQ 0.

    APPEND INITIAL LINE TO <lt_transp_data> ASSIGNING FIELD-SYMBOL(<transp_row>).

    ASSIGN COMPONENT 'COLUMNTEXT' OF STRUCTURE <transp_row> TO FIELD-SYMBOL(<transp_txt>).
    ASSIGN COMPONENT 'VALUE'      OF STRUCTURE <transp_row> TO FIELD-SYMBOL(<transp_value>).

    <transp_txt>   = <column_ref>-r_column->get_long_text( ).
    <transp_value> = <value>.

  ENDLOOP.

  "------------------------------------------------
  "-> Esportazione tabella esportata

  FIELD-SYMBOLS: <yt_data_export> TYPE table.

  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      it_fieldcatalog = lt_fcat
    IMPORTING
      ep_table        = yp_transp.

  ASSIGN yp_transp->* TO <yt_data_export>.
  <yt_data_export> = <lt_transp_data>.


ENDFORM.

