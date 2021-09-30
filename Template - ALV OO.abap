REPORT zag_simple_alv.
**********************************************************************
**********************************************************************
**********************************************************************
***************************  README   ********************************

"Mandatory Activities
*--------------------------------------------------------------------*
*) EACH CALLL SCREEN MUST BE CALLED IN THE FOLLOWING WAY, OTHERWISE THE TEMPLATE WILL NOT WORK!
"     PERFORM update_stacktrace USING sy-dynnr     "Start dynpro
"                                     c_dynnr_0100 "End dynpro.
"     CALL SCREEN 100.

*) "TODO-DDIC
"   Adapt the marked points with own DDIC 

*) "TODO-PF_TITLE
"  Create PF-STATUS called ZPF_GENERIC already set as default for dynpro into the template
"  Create PF-STATUS and TITLEBAR for your dynpro into the marked points if you want specific commands

*) Adapt PAI user command module of each created dynpro
"   If you are using ZPF_GENERIC, use module USER_COMMAND_GENERIC with OK_CODE variable to handle command
*--------------------------------------------------------------------*

"Optional Activities
*--------------------------------------------------------------------*
*) "TODO-FIELDCAT
"   Adapt fieldcat if you need into marked points

*) "TODO-HANDLER
"   Adapt your handler into the marked points

*) "TODO-EVENT_METHOD
"   Adapt the handlers method implementation into the marked points

*) "TODO-CELL_COLOR
"   Adapt the routine for color single cells
*--------------------------------------------------------------------*

**********************************************************************
**********************************************************************
**********************************************************************

"TOP
"--ALV

"Types per container e struttura Alv
TYPES: ty_ref_alv           TYPE REF TO cl_gui_alv_grid,
       ty_ref_container     TYPE REF TO cl_gui_custom_container,
       ty_ref_doc_container TYPE REF TO cl_gui_docking_container.

"Technical Type table
"used to stack the dynpro called
TYPES: BEGIN OF ty_stacktrace_dynnr,
         dynnr          TYPE sy-dynnr,
         dynnr_parent   TYPE sy-dynnr,
         event_instance TYPE ty_ref_alv,
       END OF ty_stacktrace_dynnr.

DATA: go_instance_event   TYPE ty_ref_alv,
      gt_stacktrace_dynnr TYPE TABLE OF ty_stacktrace_dynnr,
      gt_changed_data     TYPE lvc_t_modi,
      gt_toolbar          TYPE ttb_button,
      ok_code             TYPE sy-ucomm.

CONSTANTS: c_x                            VALUE 'X',
           c_e                            VALUE 'E',
           c_yes                          VALUE '1',
           c_no                           VALUE '0',
           c_icon_green     TYPE icon_d   VALUE '@5B@',
           c_icon_red       TYPE icon_d   VALUE '@5C@',
           c_icon_yell      TYPE icon_d   VALUE '@5D@',
           c_icon_info      TYPE icon_d   VALUE '@0S@',
           c_icon_miss      TYPE icon_d   VALUE '@D7@',
           c_icon_exec      TYPE icon_d   VALUE '@15@',
           c_icon_refr      TYPE icon_d   VALUE '@42@',
           c_icon_save      TYPE icon_d   VALUE '@2L@',
           c_cell_col_green TYPE lvc_col  VALUE '5',
           c_cell_col_yell  TYPE lvc_col  VALUE '3',
           c_cell_col_red   TYPE lvc_col  VALUE '6',
           c_cell_col_oran  TYPE lvc_col  VALUE '7',
           c_cell_col_null  TYPE lvc_col  VALUE '2'.
*--------------------------------------------------------------------*


"ALV SCREEN 100
*--------------------------------------------------------------------*
"Structure ALV screen 100 + Type Table
*--------------------------------------------------------------------*
TYPES: BEGIN OF ty_alv_0100.
        INCLUDE STRUCTURE t001. "TODO-DDIC
TYPES: icon  TYPE icon_d,
       msg   TYPE bapi_msg,
       c_col TYPE lvc_t_scol,
TYPES: END OF ty_alv_0100.
TYPES: tt_alv_0100 TYPE TABLE OF ty_alv_0100.

DATA: gt_alv_0100 TYPE TABLE OF ty_alv_0100	.

DATA: ok_0100               TYPE sy-ucomm,
      go_container_0100     TYPE ty_ref_container,
      go_alv_0100           TYPE ty_ref_alv,
      go_doc_container_0100 TYPE ty_ref_doc_container.

CONSTANTS: c_container_0100 TYPE scrfname      VALUE 'CONTAINER_0100',
           c_alv_st_0100    TYPE dd02l-tabname VALUE 'T001', "TODO-DDIC
           c_dynnr_0100     TYPE sy-dynnr      VALUE '0100'.
*--------------------------------------------------------------------*


"ALV SCREEN 200
*--------------------------------------------------------------------*
"Structure ALV screen 200 + Type Table
*--------------------------------------------------------------------*
TYPES: BEGIN OF ty_alv_0200.
        INCLUDE STRUCTURE lfa1. "TODO-DDIC
TYPES: icon TYPE icon_d,
       msg  TYPE bapi_msg.
TYPES: END OF ty_alv_0200.
TYPES: tt_alv_0200 TYPE TABLE OF ty_alv_0200.

DATA: gt_alv_0200 TYPE TABLE OF ty_alv_0200	.

DATA: ok_0200               TYPE sy-ucomm,
      go_container_0200     TYPE ty_ref_container,
      go_alv_0200           TYPE ty_ref_alv,
      go_doc_container_0200 TYPE ty_ref_doc_container.

CONSTANTS: c_container_0200 TYPE scrfname      VALUE 'CONTAINER_0200',
           c_alv_st_0200    TYPE dd02l-tabname VALUE 'LFA1', "TODO-DDIC
           c_dynnr_0200     TYPE sy-dynnr      VALUE '0200'.
*--------------------------------------------------------------------*


"ALV SCREEN 300
*--------------------------------------------------------------------*
"Structure ALV screen 300 + Type Table
*--------------------------------------------------------------------*
TYPES: BEGIN OF ty_alv_0300.
        INCLUDE STRUCTURE kna1. "TODO-DDIC
TYPES: icon TYPE icon_d,
       msg  TYPE bapi_msg.
TYPES: END OF ty_alv_0300.
TYPES: tt_alv_0300 TYPE TABLE OF ty_alv_0300.

DATA: gt_alv_0300 TYPE TABLE OF ty_alv_0300.

DATA: ok_0300               TYPE sy-ucomm,
      go_container_0300     TYPE ty_ref_container,
      go_alv_0300           TYPE ty_ref_alv,
      go_doc_container_0300 TYPE ty_ref_doc_container.

CONSTANTS: c_container_0300 TYPE scrfname      VALUE 'CONTAINER_0300',
           c_alv_st_0300    TYPE dd02l-tabname VALUE 'KNA1', "TODO-DDIC
           c_dynnr_0300     TYPE sy-dynnr      VALUE '0300'.
*--------------------------------------------------------------------*


"CLS
*----------------------------------------------------------------------
* CLASS cl_event_toolbar DEFINITION
*----------------------------------------------------------------------
CLASS cl_alv_event DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:
      handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object
                  e_interactive,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id
                  e_column_id
                  es_row_no,

      handle_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING
            er_data_changed.

ENDCLASS.                    "lcl_event_toolbar DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_event_toolbar IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS cl_alv_event IMPLEMENTATION.
  METHOD handle_toolbar.

    "TODO-EVENT_METHOD
    PERFORM handle_toolbar CHANGING e_object
                                    e_interactive.

  ENDMETHOD.                    "handle_toolbar
  METHOD handle_user_command.

    "TODO-EVENT_METHOD
    PERFORM handle_user_command USING e_ucomm.

  ENDMETHOD.                    "handle_user_command
  METHOD handle_hotspot_click.

    "TODO-EVENT_METHOD
    PERFORM handle_hotspot_click USING e_row_id
                                       e_column_id
                                       es_row_no.

  ENDMETHOD.                    "handle_hotspot_click
  METHOD handle_data_changed.

    "TODO-EVENT_METHOD
    PERFORM handle_data_changed CHANGING er_data_changed.

  ENDMETHOD.                    "handle_data_changed

ENDCLASS.                    "lcl_event_toolbar IMPLEMENTATION

TYPES ty_ref_alv_event TYPE REF TO cl_alv_event.
DATA: go_alv_event     TYPE ty_ref_alv_event.


*--------------------------------------------------------------------*
*--------------------------------------------------------------------*
"ALV BUILDING IMPLEMENTATION
*&---------------------------------------------------------------------*
*& Form UPDATE_STACKTRACE
*&---------------------------------------------------------------------*
FORM update_stacktrace  USING x_parent_dynnr  TYPE sy-dynnr
                              x_calling_dynnr TYPE sy-dynnr.

  FIELD-SYMBOLS: <stacktrace> LIKE LINE OF gt_stacktrace_dynnr.

  UNASSIGN <stacktrace>.
  READ TABLE gt_stacktrace_dynnr WITH KEY dynnr        = x_calling_dynnr
                                          dynnr_parent = x_parent_dynnr
                                          TRANSPORTING NO FIELDS.
  IF sy-subrc <> 0.
    APPEND INITIAL LINE TO gt_stacktrace_dynnr ASSIGNING <stacktrace>.
    <stacktrace>-dynnr        = x_calling_dynnr.
    <stacktrace>-dynnr_parent = x_parent_dynnr.
  ENDIF.

ENDFORM.                    "update_stacktrace
*&---------------------------------------------------------------------*
*&      Form  PRINT_ALV
*&---------------------------------------------------------------------*
FORM print_alv USING    x_cont_name      TYPE scrfname
                        x_structure      TYPE dd02l-tabname
                        xt_table         TYPE STANDARD TABLE
               CHANGING yo_container     TYPE ty_ref_container
                        yo_alv_ref       TYPE ty_ref_alv
                        yo_doc_container TYPE ty_ref_doc_container.

  DATA: lt_fcat   TYPE lvc_t_fcat,
        ls_layout TYPE lvc_s_layo.

  FIELD-SYMBOLS: <stacktrace> LIKE LINE OF gt_stacktrace_dynnr.

*--------------------------------------------------------------------*

  PERFORM init_container USING    x_cont_name
                         CHANGING yo_container
                                  yo_alv_ref
                                  yo_doc_container.

  PERFORM init_fieldcat USING    x_structure
                        CHANGING lt_fcat[].

  PERFORM init_layout USING xt_table
                      CHANGING ls_layout.

  UNASSIGN <stacktrace>.
  READ TABLE gt_stacktrace_dynnr ASSIGNING <stacktrace>
    WITH KEY dynnr = sy-dynnr.
  IF sy-subrc EQ 0.
    <stacktrace>-event_instance = yo_alv_ref.
    go_instance_event = yo_alv_ref.
  ENDIF.

  PERFORM init_handlers CHANGING yo_alv_ref.


  CALL METHOD yo_alv_ref->set_table_for_first_display
    EXPORTING
      i_save                        = 'A'
      is_layout                     = ls_layout
    CHANGING
      it_outtab                     = xt_table[] "<dyn_table>
      it_fieldcatalog               = lt_fcat[]
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM.                    " PRINT_ALV
*&---------------------------------------------------------------------*
*&      Form  REFRESH_TABLE
*&---------------------------------------------------------------------*
FORM refresh_table USING p_ref_alv TYPE ty_ref_alv.

  DATA lt_stable TYPE lvc_s_stbl.

  lt_stable-row = 'X'.
  lt_stable-col = 'X'.

  CALL METHOD p_ref_alv->refresh_table_display
    EXPORTING
      is_stable      = lt_stable
      i_soft_refresh = 'X'.

ENDFORM.                    " REFRESH_TABLE
*&---------------------------------------------------------------------*
*&      Form  INIT_CONTAINER
*&---------------------------------------------------------------------*
FORM init_container  USING    x_cont_name      TYPE scrfname
                     CHANGING yo_container     TYPE ty_ref_container
                              yo_alv_ref       TYPE ty_ref_alv
                              yo_doc_container TYPE ty_ref_doc_container.


  IF yo_container IS INITIAL.
    IF cl_gui_alv_grid=>offline( ) IS INITIAL.
      CREATE OBJECT yo_container
        EXPORTING
          container_name = x_cont_name.

      CREATE OBJECT yo_alv_ref
        EXPORTING
          i_parent = yo_container.
    ELSE.
* If it is in background:
      CREATE OBJECT yo_alv_ref
        EXPORTING
          i_parent = yo_doc_container.

    ENDIF.

  ELSE.

    PERFORM refresh_table USING yo_alv_ref.

  ENDIF.

ENDFORM.                    " INIT_CONTAINER
*&---------------------------------------------------------------------*
*&      Form  INIT_FIELDCAT
*&---------------------------------------------------------------------*
FORM init_fieldcat USING    x_structure TYPE dd02l-tabname
                   CHANGING yt_fcat     TYPE lvc_t_fcat.

  DATA: ls_fcat TYPE lvc_s_fcat.

  FIELD-SYMBOLS: <fcat> LIKE LINE OF yt_fcat.

*--------------------------------------------------------------------*
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = x_structure
    CHANGING
      ct_fieldcat            = yt_fcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

  ls_fcat-col_opt = 'X'.
  MODIFY yt_fcat FROM ls_fcat TRANSPORTING col_opt
                                     WHERE fieldname NE 'ICON'.

  "TODO-FIELDCAT
  "TO add new fields manually
  CASE sy-dynnr.
    WHEN c_dynnr_0100.

*      UNASSIGN <fcat>.
*      APPEND INITIAL LINE TO yt_fcat ASSIGNING <fcat>.
*      <fcat>-col_pos   = '0' .
*      <fcat>-fieldname = 'ICON' .
*      <fcat>-seltext   = 'Stato' .
*      <fcat>-scrtext_l = 'Stato' .
*      <fcat>-scrtext_m = 'Stato' .
*      <fcat>-scrtext_s = 'Stato' .
*      <fcat>-key       = ' ' .

*      UNASSIGN <fcat>.
*      APPEND INITIAL LINE TO yt_fcat ASSIGNING <fcat>.
*      <fcat>-col_pos   = '0' .
*      <fcat>-fieldname = 'MSG' .
*      <fcat>-seltext   = 'Esito Elaborazione' .
*      <fcat>-scrtext_l = 'Esito Elaborazione' .
*      <fcat>-scrtext_m = 'Esito Elab.' .
*      <fcat>-scrtext_s = 'Esito' .
*      <fcat>-key       = ' ' .

    WHEN c_dynnr_0200.

    WHEN c_dynnr_0100.

    WHEN OTHERS.
  ENDCASE.

  "TODO-FIELDCAT
  "Change properties of existing fcat fields
  LOOP AT yt_fcat ASSIGNING <fcat>.
    CASE sy-dynnr.
      WHEN c_dynnr_0100.

        CASE <fcat>-fieldname.
          WHEN ''.
          WHEN OTHERS.
        ENDCASE.

      WHEN c_dynnr_0200.

      WHEN c_dynnr_0100.

      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " INIT_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  INIT_LAYOUT
*&---------------------------------------------------------------------*
FORM init_layout  USING    xt_table  TYPE STANDARD TABLE
                  CHANGING ys_layout TYPE lvc_s_layo.

  FIELD-SYMBOLS: <row>       TYPE any,
                 <component> TYPE any.

*--------------------------------------------------------------------*
  ys_layout-sel_mode   = 'A'.
  ys_layout-zebra      = 'X'.

  UNASSIGN <row>.
  READ TABLE xt_table ASSIGNING <row> INDEX 1.
  IF <row> IS ASSIGNED.

    UNASSIGN <component>.
    ASSIGN COMPONENT 'C_COL' OF STRUCTURE <row> TO <component>.
    IF <component> IS ASSIGNED.

      ys_layout-ctab_fname = 'C_COL'.

    ENDIF.
  ENDIF.

ENDFORM.                    " INIT_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  INIT_HANDLERS
*&---------------------------------------------------------------------*
FORM init_handlers  CHANGING yo_alv_ref TYPE ty_ref_alv.

  IF sy-batch EQ space.
    IF go_instance_event IS NOT INITIAL.

      "TODO-HANDLER
      CALL METHOD yo_alv_ref->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_enter.

      CREATE OBJECT go_alv_event.
      SET HANDLER go_alv_event->handle_user_command    FOR yo_alv_ref.
      SET HANDLER go_alv_event->handle_toolbar         FOR yo_alv_ref.
      SET HANDLER go_alv_event->handle_hotspot_click   FOR yo_alv_ref.
      SET HANDLER go_alv_event->handle_data_changed    FOR yo_alv_ref.

    ENDIF.
  ENDIF.

ENDFORM.                    " INIT_HANDLERS
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_GENERIC  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_generic INPUT.

  DATA: lv_answer,
        lv_cur_field(20),
        lv_cur_val(100).

  FIELD-SYMBOLS: <stacktrace> LIKE LINE OF gt_stacktrace_dynnr,
                 <parent>     LIKE LINE OF gt_stacktrace_dynnr.

  CASE ok_code.
    WHEN '&F03'
      OR 'BACK'.

      PERFORM check_if_not_saved_data CHANGING lv_answer.
      CHECK lv_answer EQ space
         OR lv_answer EQ c_yes.

      UNASSIGN <stacktrace>.
      READ TABLE gt_stacktrace_dynnr ASSIGNING <stacktrace>
        WITH KEY dynnr = sy-dynnr.
      IF sy-subrc EQ 0.

        UNASSIGN <parent>.
        READ TABLE gt_stacktrace_dynnr ASSIGNING <parent>
          WITH KEY dynnr = <stacktrace>-dynnr_parent.
        IF sy-subrc EQ 0.
          go_instance_event = <parent>-event_instance.
        ENDIF.

      ENDIF.

      LEAVE TO SCREEN 0.

    WHEN '&F12'
      OR '&F15'
      OR 'CANC'
      OR 'EXIT'.
      LEAVE PROGRAM.

    WHEN 'PICK'.

  ENDCASE.

ENDMODULE.                    "user_command_generic INPUT
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

*  SET PF-STATUS 'ZPF_0100'.    "TODO-PF_TITLE
  SET PF-STATUS 'ZPF_GENERIC'.
  SET TITLEBAR 'ZTIT_0100'.

  PERFORM print_alv USING c_container_0100
                          c_alv_st_0100
                          gt_alv_0100[]
                 CHANGING go_container_0100
                          go_alv_0100
                          go_doc_container_0100 .

ENDMODULE.                    "handle_hotspot_click
*&---------------------------------------------------------------------*
*& Module STATUS_0200 OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0200 OUTPUT.

*  SET PF-STATUS 'ZPF_0200'.    "TODO-PF_TITLE
  SET PF-STATUS 'ZPF_GENERIC'.
  SET TITLEBAR 'ZTIT_0200'.

  PERFORM print_alv USING c_container_0200
                          c_alv_st_0200
                          gt_alv_0200[]
                 CHANGING go_container_0200
                          go_alv_0200
                          go_doc_container_0200 .

ENDMODULE.                    "handle_hotspot_click
*&---------------------------------------------------------------------*
*& Module STATUS_0300 OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0300 OUTPUT.

*  SET PF-STATUS 'ZPF_0300'.    "TODO-PF_TITLE
  SET PF-STATUS 'ZPF_GENERIC'.
  SET TITLEBAR 'ZTIT_0300'.

  PERFORM print_alv USING c_container_0300
                          c_alv_st_0300
                          gt_alv_0300[]
                 CHANGING go_container_0300
                          go_alv_0300
                          go_doc_container_0300 .

ENDMODULE.                    "handle_hotspot_click
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
"CLASS FUNCTIONS IMPMLEMENTATION
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form CHECK_IF_NOT_SAVED_DATA
*&---------------------------------------------------------------------*
FORM check_if_not_saved_data  CHANGING y_answer.

  CLEAR y_answer.

  IF gt_changed_data[] IS NOT INITIAL.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        text_question  = 'Dati non salvati. Procedere comunque?'
      IMPORTING
        answer         = y_answer
      EXCEPTIONS
        text_not_found = 1
        OTHERS         = 2.
  ENDIF.

  CHECK y_answer EQ c_yes.

  "Clear changed data if exit without save (YES)
  CLEAR gt_changed_data[].

ENDFORM.                    "check_if_not_Saved_data
*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
FORM handle_hotspot_click USING x_row_id    TYPE lvc_s_row
                                x_column_id TYPE lvc_s_col
                                xs_row_no   TYPE lvc_s_roid.

  FIELD-SYMBOLS: <alv_0100> LIKE LINE OF gt_alv_0100.

*--------------------------------------------------------------------*

  CASE sy-dynnr.
    WHEN c_dynnr_0100.
      READ TABLE gt_alv_0100 ASSIGNING <alv_0100> INDEX xs_row_no-row_id.
      CHECK sy-subrc EQ 0.
      CASE x_column_id.

        WHEN 'ANLAGE'.
*      CHECK <alv_0100>-anlage IS NOT INITIAL.
*      SET PARAMETER ID 'ANL'
*                 FIELD <alv_0100>-anlage.
*      CALL TRANSACTION 'ES32' AND SKIP FIRST SCREEN.


      ENDCASE.

    WHEN c_dynnr_0200.

    WHEN c_dynnr_0300.

  ENDCASE.


ENDFORM.                    " HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*&      Form  HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
FORM handle_toolbar  CHANGING y_object      TYPE REF TO cl_alv_event_toolbar_set
                              y_interactive TYPE  char01.

  DATA: lw_toolbar TYPE stb_button.

  CASE sy-dynnr.
    WHEN c_dynnr_0100.
      "BTN
      CLEAR lw_toolbar.
      lw_toolbar-function  = '&BTN'.
      lw_toolbar-icon      = c_icon_exec.
      lw_toolbar-butn_type = '0'.
      lw_toolbar-text      = 'Function'.
      lw_toolbar-quickinfo = 'Function'.
      APPEND lw_toolbar TO y_object->mt_toolbar.
      
      CLEAR lw_toolbar.
      lw_toolbar-function  = '&SAVE_0100'.
      lw_toolbar-icon      = c_icon_save.
      lw_toolbar-butn_type = '0'.
      lw_toolbar-text      = 'Save Data'.
      lw_toolbar-quickinfo = 'Save Data'.
      APPEND lw_toolbar TO y_object->mt_toolbar.
      
      CLEAR lw_toolbar.
      lw_toolbar-function  = '&SCREEN_0200'.
      lw_toolbar-icon      = c_icon_exec.
      lw_toolbar-butn_type = '0'.
      lw_toolbar-text      = 'Screen 0200'.
      lw_toolbar-quickinfo = 'Screen 0200'.
      APPEND lw_toolbar TO y_object->mt_toolbar.

    WHEN c_dynnr_0200.

    WHEN c_dynnr_0300.

    WHEN OTHERS.
  ENDCASE.


ENDFORM.                    " HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
*&      Form  HANDLE_USER_COMMAND
*&---------------------------------------------------------------------*
FORM handle_user_command  USING x_ucomm TYPE sy-ucomm.

  DATA: ls_alv             LIKE LINE OF gt_alv_0100,
            lt_selected_rows   TYPE lvc_t_row.

  FIELD-SYMBOLS: <sel_rows> LIKE LINE OF lt_selected_rows.       
       
  REFRESH lt_selected_rows.
  CALL METHOD go_instance_event->get_selected_rows
     IMPORTING
       et_index_rows = lt_selected_rows.
   IF lt_selected_rows[] IS NOT INITIAL.

   ELSE.
*     MESSAGE s000(db) WITH 'Select at least one row'
*                         DISPLAY LIKE c_e.
*     EXIT.
  ENDIF.
      
  CASE x_ucomm.
    WHEN '&BTN'.
*       PERFORM command_ USING lt_selected_row.

    WHEN '&SAVE_0100'.
       PERFORM PERFORM command_save_data_0100 USING gt_changed_data[].
    
    WHEN '&SCREEN_0200'.
      PERFORM update_stacktrace USING sy-dynnr c_dynnr_0100.
      CALL SCREEN 200.
      
  ENDCASE.
  
  PERFORM refresh_table USING go_instance_event.

ENDFORM.                    " HANDLE_USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  HANDLE_DATA_CHANGED
*&---------------------------------------------------------------------*
FORM handle_data_changed  CHANGING yr_data_changed  TYPE REF TO cl_alv_changed_data_protocol.

  IF yr_data_changed->mt_good_cells[] IS NOT INITIAL.
    APPEND LINES OF yr_data_changed->mt_good_cells[] TO gt_changed_data[].
  ENDIF.

ENDFORM.                    " HANDLE_DATA_CHANGED
*&---------------------------------------------------------------------*
*& Form COMMAND_SAVE_DATA_0100
*&---------------------------------------------------------------------*
FORM command_save_data_0100  USING xt_changed_data TYPE lvc_t_modi.

  LOOP AT xt_changed_data ASSIGNING FIELD-SYMBOL(<chng>).
    READ TABLE gt_alv_0100 ASSIGNING FIELD-SYMBOL(<alv_0100>)
                                            INDEX <chng>-row_id.
    CHECK sy-subrc EQ 0.

    ASSIGN COMPONENT <chng>-fieldname OF STRUCTURE <alv_0100>
      TO FIELD-SYMBOL(<value>).
    CHECK <value> IS ASSIGNED.

    <value> = <chng>-value.

  ENDLOOP.

  CLEAR gt_changed_data[].

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILL_CELL_COLORS
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form FILL_CELL_COLORS
*&---------------------------------------------------------------------*
FORM fill_cell_colors  USING    x_fieldname TYPE tabname
                                x_struct    TYPE tabname
                       CHANGING y_struct    TYPE any.
   
  "TODO-CELL_COLOR

  FIELD-SYMBOLS: <alv_0100> LIKE LINE OF gt_alv_0100,
                 <alv_0200> LIKE LINE OF gt_alv_0200,
                 <alv_0300> LIKE LINE OF gt_alv_0300.

  FIELD-SYMBOLS: <value> TYPE any.

  UNASSIGN: <value>,
            <alv_0100>, <alv_0200>, <alv_0300>.

  CASE x_struct.
    WHEN c_alv_st_0100.
      ASSIGN y_struct TO <alv_0100>.

      CHECK <alv_0100> IS ASSIGNED.

      DELETE <alv_0100>-c_col[] WHERE fname EQ x_fieldname.

      ASSIGN COMPONENT x_fieldname OF STRUCTURE y_struct TO <value>.
      CHECK <value> IS ASSIGNED.

      APPEND INITIAL LINE TO <alv_0100>-c_col[] ASSIGNING FIELD-SYMBOL(<color>).
      <color>-fname = x_fieldname.
      <color>-color-int = '1' . "intensified on 1 / 0 off

      IF <value> EQ ''.
        <color>-color-col = c_cell_col_red .
      ELSE.
        <color>-color-col = c_cell_col_yell .
      ENDIF.


    WHEN c_dynnr_0200.

    WHEN c_dynnr_0300.

    WHEN OTHERS.
  ENDCASE.

ENDFORM.



START-OF-SELECTION.

  SELECT * UP TO 10 ROWS
    FROM t001 INTO TABLE gt_alv_0100.

  SELECT * UP TO 10 ROWS
  FROM lfa1 INTO TABLE gt_alv_0200.

  SELECT * UP TO 10 ROWS
FROM kna1 INTO TABLE gt_alv_0300.

  DATA: ls_alv_0100 LIKE LINE OF gt_alv_0100.
  DATA: ls_alv_0200 LIKE LINE OF gt_alv_0200.
  DATA: ls_alv_0300 LIKE LINE OF gt_alv_0300.
  ls_alv_0100-icon = c_icon_info.
  ls_alv_0100-msg  = 'Test Flight'.

  MODIFY gt_alv_0100 FROM ls_alv_0100 TRANSPORTING icon
                                                   msg
                                             WHERE icon EQ space.

  PERFORM update_stacktrace USING sy-dynnr c_dynnr_0100.
  CALL SCREEN 100.
