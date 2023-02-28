* https://github.com/boy0korea/ZFPM_TOOLS

REPORT zfpm_tools_xml_edit.

**********************************************************************
* global variable
**********************************************************************
DATA: go_dock             TYPE REF TO cl_gui_docking_container,
      go_edit             TYPE REF TO cl_gui_textedit,
      gv_readonly         TYPE flag,
      gs_wdy_config_data  TYPE wdy_config_data,
      gs_wdy_config_appl  TYPE wdy_config_appl,
      gs_wdy_config_key   TYPE wdy_config_key,
      gs_enhheader        TYPE enhheader,
      gs_wdy_cfg_enh_data TYPE wdy_cfg_enh_data,
      gv_str              TYPE string,
      ok_code             TYPE okcode.
FIELD-SYMBOLS: <gv_xstr> TYPE xstring.

**********************************************************************
* SCREEN
**********************************************************************
* line 1 : WDCC
SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS: pr_wdcc TYPE c RADIOBUTTON GROUP typ DEFAULT 'X' USER-COMMAND typ.
  SELECTION-SCREEN COMMENT (28) FOR FIELD pr_wdcc.
  PARAMETERS: pa_wdcc TYPE wdy_config_data-config_id MODIF ID cc MATCHCODE OBJECT wd_conf_comp.
SELECTION-SCREEN END OF LINE.
* line 2 : WDCA
SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS: pr_wdca TYPE c RADIOBUTTON GROUP typ.
  SELECTION-SCREEN COMMENT (28) FOR FIELD pr_wdca.
  PARAMETERS: pa_wdca TYPE wdy_config_appl-config_id MODIF ID ca MATCHCODE OBJECT wd_conf_appl.
SELECTION-SCREEN END OF LINE.
* line 3 : ENHO on WDCC
SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS: pr_enho TYPE c RADIOBUTTON GROUP typ.
  SELECTION-SCREEN COMMENT (28) FOR FIELD pr_enho.
  PARAMETERS: pa_enho TYPE enhname MODIF ID en.
SELECTION-SCREEN END OF LINE.

PARAMETERS: p_edit TYPE flag DEFAULT 'X' NO-DISPLAY.

**********************************************************************
INITIALIZATION.
**********************************************************************
  PERFORM init.

**********************************************************************
AT SELECTION-SCREEN OUTPUT.
**********************************************************************
  PERFORM modify_screen.

**********************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_enho.
**********************************************************************
  PERFORM f4_pa_enho.

**********************************************************************
START-OF-SELECTION.
**********************************************************************
  PERFORM exec.

FORM init.
ENDFORM.
FORM modify_screen.
  LOOP AT SCREEN.
    IF pr_wdcc EQ 'X'.
      IF screen-group1 EQ 'CC'.
        screen-active = '1'.
        screen-invisible = '0'.
        MODIFY SCREEN.
        SET CURSOR FIELD 'PA_WDCC'.
      ELSEIF screen-group1 EQ 'CA' OR screen-group1 EQ 'EN'.
        screen-active = '0'.
        screen-invisible = '1'.
        MODIFY SCREEN.
      ENDIF.
    ELSEIF pr_wdca EQ 'X'.
      IF screen-group1 EQ 'CA'.
        screen-active = '1'.
        screen-invisible = '0'.
        MODIFY SCREEN.
        SET CURSOR FIELD 'PA_WDCA'.
      ELSEIF screen-group1 EQ 'CC' OR screen-group1 EQ 'EN'.
        screen-active = '0'.
        screen-invisible = '1'.
        MODIFY SCREEN.
      ENDIF.
    ELSEIF pr_enho EQ 'X'.
      IF screen-group1 EQ 'EN'.
        screen-active = '1'.
        screen-invisible = '0'.
        MODIFY SCREEN.
        SET CURSOR FIELD 'PA_ENHO'.
      ELSEIF screen-group1 EQ 'CC' OR screen-group1 EQ 'CA'.
        screen-active = '0'.
        screen-invisible = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.
FORM f4_pa_enho .
  DATA: lt_enho_value_tab TYPE TABLE OF wdy_config_enho_se80,
        ls_enho_value_tab TYPE wdy_config_enho_se80,
        lv_fldvalue       TYPE help_info-fldvalue.

  SELECT enhname shorttext_id AS description
    INTO CORRESPONDING FIELDS OF TABLE lt_enho_value_tab
    FROM enhheader
    WHERE enhtooltype = 'WDYCONF'
      AND version = 'A'
  .
  LOOP AT lt_enho_value_tab INTO ls_enho_value_tab.
    " description
    SELECT SINGLE text
      INTO ls_enho_value_tab-description
      FROM sotr_text
      WHERE concept = ls_enho_value_tab-description
        AND langu = sy-langu
    .
    " WDCC key
    CLEAR: gs_wdy_config_key.
    SELECT SINGLE obj_name
      INTO gs_wdy_config_key
      FROM enhobj
      WHERE enhname = ls_enho_value_tab-enhname
        AND version = 'A'
    .
    MOVE-CORRESPONDING gs_wdy_config_key TO ls_enho_value_tab.

    MODIFY lt_enho_value_tab FROM ls_enho_value_tab.
  ENDLOOP.

  lv_fldvalue = pa_enho.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     DDIC_STRUCTURE  = ' '
      retfield        = 'ENHNAME'
*     PVALKEY         = ' '
      dynpprog        = sy-repid
      dynpnr          = '1000'
      dynprofield     = 'PA_ENHO'
*     STEPL           = 0
*     WINDOW_TITLE    = WINDOW_TITLE
      value           = lv_fldvalue
      value_org       = 'S'
*     MULTIPLE_CHOICE = ' '
*     DISPLAY         = ' '
*     CALLBACK_PROGRAM       = ' '
*     CALLBACK_FORM   = ' '
*     CALLBACK_METHOD = CALLBACK_METHOD
*     MARK_TAB        = MARK_TAB
* IMPORTING
*     USER_RESET      = USER_RESET
    TABLES
      value_tab       = lt_enho_value_tab
*     FIELD_TAB       = FIELD_TAB
*     RETURN_TAB      = RETURN_TAB
*     DYNPFLD_MAPPING = DYNPFLD_MAPPING
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
FORM exec.

  PERFORM init_go_dock.
  PERFORM load_xml.

  IF gs_wdy_config_key IS NOT INITIAL.
    CALL SCREEN 10.
*PROCESS BEFORE OUTPUT.
*  MODULE pbo_0010.
*  MODULE status_0010.
*
*PROCESS AFTER INPUT.
*  MODULE user_command_0010.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Module  PBO_0010  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_0010 OUTPUT.
ENDMODULE.                 " PBO_0010  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0010  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0010 OUTPUT.
  IF gv_readonly EQ abap_false.
    SET PF-STATUS 'ST_10'.
  ELSE.
    SET PF-STATUS 'ST_10' EXCLUDING 'SAVE'.
  ENDIF.

* Title: &1 : &2 &3 &4
  IF pr_wdcc EQ 'X'.
    SET TITLEBAR 'TI_10' WITH 'WDCC' gs_wdy_config_key.
  ELSEIF pr_wdca EQ 'X'.
    SET TITLEBAR 'TI_10' WITH 'WDCA' gs_wdy_config_key.
  ELSEIF pr_enho EQ 'X'.
    SET TITLEBAR 'TI_10' WITH 'ENHO' pa_enho 'on' gs_wdy_config_key.
  ENDIF.

ENDMODULE.                 " STATUS_0010  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0010  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0010 INPUT.
  CASE ok_code.
    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
*      LEAVE PROGRAM.
      PERFORM dequeue.
      LEAVE TO SCREEN 0.
    WHEN 'SAVE'.
      PERFORM do_save.
      PERFORM dequeue.
      LEAVE TO SCREEN 0.
    WHEN 'SE80'.
      PERFORM do_se80.
    WHEN 'WEB_CFG'.
      PERFORM do_web_cfg.
    WHEN 'RELOAD'.
      PERFORM load_xml.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0010  INPUT
*&---------------------------------------------------------------------*
*&      Form  DO_SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM do_save .
  CLEAR: gv_str.
  go_edit->get_textstream(
    IMPORTING
      text                   = gv_str    " Text as String with Carriage Returns and Linefeeds
    EXCEPTIONS
      error_cntl_call_method = 1
      not_supported_by_gui   = 2
      OTHERS                 = 3
  ).
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  cl_gui_cfw=>flush( ).

  <gv_xstr> = cl_wdr_configuration_utils=>xml_string2xstring( in_string = gv_str ).

  IF pr_wdcc EQ 'X'.

    UPDATE wdy_config_data FROM gs_wdy_config_data.
    " invalidate shared object
    cl_wdr_cfg_comp_shbroker=>invalidate(
        scope          = cl_wdr_cfg_comp_shbroker=>c_scope_config
        component_name = gs_wdy_config_data-component
        config_key     = gs_wdy_config_key
    ).
  ELSEIF pr_wdca EQ 'X'.
    UPDATE wdy_config_appl FROM gs_wdy_config_appl.
    " invalidate shared object
    cl_wdr_conf_appl_shbroker=>invalidate(
      EXPORTING
        scope            = cl_wdr_config_appl_param=>c_scope_config
        application_name = gs_wdy_config_appl-application
        config_id        = gs_wdy_config_appl-config_id
    ).
  ELSEIF pr_enho EQ 'X'.
    TRY.
        cl_enh_utilities_xstring=>store_data(
          EXPORTING
            pi_input   = gs_wdy_cfg_enh_data
          CHANGING
            pe_xstring = gs_enhheader-data
        ).
        UPDATE enhheader FROM gs_enhheader.
      CATCH cx_enh_no_valid_input_type.    " Enhancement Persistence for Standard Tables
    ENDTRY.
    " get component id
    SELECT SINGLE component
      INTO gs_wdy_config_data-component
      FROM wdy_config_data
      WHERE config_id = gs_wdy_config_key-config_id
        AND config_type = gs_wdy_config_key-config_type
        AND config_var = gs_wdy_config_key-config_var
    .
    " invalidate shared object
    cl_wdr_cfg_comp_shbroker=>invalidate(
        scope          = cl_wdr_cfg_comp_shbroker=>c_scope_config
        component_name = gs_wdy_config_data-component
        config_key     = gs_wdy_config_key
    ).
  ENDIF.

  COMMIT WORK.

ENDFORM.                    " DO_SAVE
*&---------------------------------------------------------------------*
*&      Form  DO_SE80
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM do_se80 .
  DATA: lv_object_name TYPE char40,
        lv_object_type TYPE char4.

  IF pr_wdcc EQ 'X'.
    lv_object_name = gs_wdy_config_key.
    lv_object_type = 'WDCC'.
  ELSEIF pr_wdca EQ 'X'.
    lv_object_name = gs_wdy_config_key.
    lv_object_type = 'WDCA'.
  ELSEIF pr_enho EQ 'X'.
    lv_object_name = pa_enho.
    lv_object_type = 'ENHO'.
  ENDIF.

  CALL FUNCTION 'RS_TOOL_ACCESS'
    EXPORTING
      operation           = 'SHOW'    " Operation
      object_name         = lv_object_name    " Object Name
      object_type         = lv_object_type    " Object Type
    EXCEPTIONS
      not_executed        = 1
      invalid_object_type = 2
      OTHERS              = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " DO_SE80
*&---------------------------------------------------------------------*
*&      Form  DO_WEB_CFG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM do_web_cfg .

  DATA: lv_application TYPE wdy_application_name,
        lt_parameter   TYPE tihttpnvp,
        ls_parameter   TYPE ihttpnvp.

  IF pr_wdcc EQ 'X'.
    lv_application = 'CONFIGURE_COMPONENT'.

    CLEAR: lt_parameter, ls_parameter.
    ls_parameter-name = 'COMP_NAME'.
    ls_parameter-value = gs_wdy_config_data-component.
    APPEND ls_parameter TO lt_parameter.
    CLEAR: ls_parameter.
    ls_parameter-name = 'ACTION'.
    ls_parameter-value = 'CHANGE'.
    APPEND ls_parameter TO lt_parameter.
  ELSEIF pr_wdca EQ 'X'.
    lv_application = 'CONFIGURE_APPLICATION'.

    CLEAR: lt_parameter, ls_parameter.
    ls_parameter-name = 'APPL_NAME'.
    ls_parameter-value = gs_wdy_config_appl-application.
    APPEND ls_parameter TO lt_parameter.
    CLEAR: ls_parameter.
    ls_parameter-name = 'ACTION'.
    ls_parameter-value = 'CHANGE'.
    APPEND ls_parameter TO lt_parameter.
  ELSEIF pr_enho EQ 'X'.
    lv_application = 'CONFIGURE_COMPONENT'.

    CLEAR: lt_parameter, ls_parameter.
    ls_parameter-name = 'COMP_NAME'.
    SELECT SINGLE component
      INTO ls_parameter-value
      FROM wdy_config_data
      WHERE config_id = gs_wdy_config_key-config_id
        AND config_type = gs_wdy_config_key-config_type
        AND config_var = gs_wdy_config_key-config_var
    .
    APPEND ls_parameter TO lt_parameter.
    CLEAR: ls_parameter.
    ls_parameter-name = 'ACTION'.
    ls_parameter-value = 'DISPLAY'.
    APPEND ls_parameter TO lt_parameter.
  ENDIF.

  CLEAR: ls_parameter.
  ls_parameter-name = 'CONFIG_ID'.
  ls_parameter-value = gs_wdy_config_key-config_id.
  APPEND ls_parameter TO lt_parameter.
  CLEAR: ls_parameter.
  ls_parameter-name = 'CONFIG_TYPE'.
  ls_parameter-value = gs_wdy_config_key-config_type.
  APPEND ls_parameter TO lt_parameter.
  CLEAR: ls_parameter.
  ls_parameter-name = 'CONFIG_VAR'.
  ls_parameter-value = gs_wdy_config_key-config_var.
  APPEND ls_parameter TO lt_parameter.

  CALL FUNCTION 'WDY_EXECUTE_IN_PLACE'
    EXPORTING
*     protocol            = protocol
*     internalmode        = 'X'
*     smartclient         = smartclient
      application         = lv_application
*     container_name      = container_name
      parameters          = lt_parameter
*     suppress_output     = suppress_output
*    IMPORTING
*     out_url             = out_url
*    CHANGING
*     viewer              = viewer
    EXCEPTIONS
      invalid_application = 1
      browser_not_started = 2
      action_cancelled    = 3
      OTHERS              = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " DO_WEB_CFG
*&---------------------------------------------------------------------*
*&      Form  INIT_GO_DOCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_go_dock .
  IF go_dock IS INITIAL.
    CREATE OBJECT go_dock
      EXPORTING
*       parent                      = parent    " Parent container
        repid                       = sy-repid    " Report to Which This Docking Control is Linked
        dynnr                       = '0010'    " Screen to Which This Docking Control is Linked
*       side                        = DOCK_AT_LEFT    " Side to Which Control is Docked
*       extension                   = 50    " Control Extension
*       style                       = style    " Windows Style Attributes Applied to This Docking Container
*       lifetime                    = LIFETIME_DEFAULT    " Lifetime
*       caption                     = caption    " Caption
*       metric                      = 0    " Metric
        ratio                       = 95    " Percentage of Screen: Takes Priority Over EXTENSION
*       no_autodef_progid_dynnr     = no_autodef_progid_dynnr    " Don't Autodefined Progid and Dynnr?
*       name                        = name    " Name
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CREATE OBJECT go_edit
      EXPORTING
*       max_number_chars       = max_number_chars    " maximum number of characters insertable into editor control
*       style                  = 0    " control style, if initial a defined value is choosen
*       wordwrap_mode          = WORDWRAP_AT_WINDOWBORDER    " 0: OFF; 1: wrap a window border; 2: wrap at fixed position
*       wordwrap_position      = -1    " position of wordwrap, only makes sense with wordwrap_mode=2
*       wordwrap_to_linebreak_mode = FALSE    " eq 1: change wordwrap to linebreak; 0: preserve wordwraps
*       filedrop_mode          = DROPFILE_EVENT_OFF    " event mode to handle drop of files on control
        parent                 = go_dock    " Parent Container
*       lifetime               = lifetime    " for life time management
*       name                   = name    " name for the control
      EXCEPTIONS
        error_cntl_create      = 1
        error_cntl_init        = 2
        error_cntl_link        = 3
        error_dp_create        = 4
        gui_type_not_supported = 5
        OTHERS                 = 6.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

ENDFORM.                    " INIT_GO_DOCK
*&---------------------------------------------------------------------*
*&      Form  LOAD_XML
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM load_xml .

  CLEAR: gs_wdy_config_data, gs_wdy_config_appl, gs_wdy_config_key, gv_str, gv_readonly.

  IF pr_wdcc EQ 'X'.
    SELECT SINGLE *
      INTO gs_wdy_config_data
      FROM wdy_config_data
      WHERE config_id = pa_wdcc
        AND config_type = '00'
        AND config_var = ''
    .
    ASSIGN COMPONENT 'XCONTENT' OF STRUCTURE gs_wdy_config_data TO <gv_xstr>.
    MOVE-CORRESPONDING gs_wdy_config_data TO gs_wdy_config_key.
  ELSEIF pr_wdca EQ 'X'.
    SELECT SINGLE *
      INTO gs_wdy_config_appl
      FROM wdy_config_appl
      WHERE config_id = pa_wdca
        AND config_type = '02'
        AND config_var = ''
    .
    ASSIGN COMPONENT 'XCONTENT' OF STRUCTURE gs_wdy_config_appl TO <gv_xstr>.
    MOVE-CORRESPONDING gs_wdy_config_appl TO gs_wdy_config_key.
  ELSEIF pr_enho EQ 'X'.
    " WDCC key
    SELECT SINGLE obj_name
      INTO gs_wdy_config_key
      FROM enhobj
      WHERE enhname = pa_enho
        AND version = 'A'
    .
    " ENHHEADER-DATA
    SELECT SINGLE *
      INTO gs_enhheader
      FROM enhheader
      WHERE enhname = pa_enho
        AND version = 'A'
    .
    cl_enh_utilities_xstring=>get_data(
      EXPORTING
        pi_xstring = gs_enhheader-data
      IMPORTING
        pe_output  = gs_wdy_cfg_enh_data
    ).
    " gs_wdy_cfg_enh_data-xmlxstring
    ASSIGN COMPONENT 'XMLXSTRING' OF STRUCTURE gs_wdy_cfg_enh_data TO <gv_xstr>.
  ENDIF.


  IF gs_wdy_config_key IS NOT INITIAL.
    PERFORM enqueue.

    gv_str = cl_wdr_configuration_utils=>xml_xstring2string( in_xstring = <gv_xstr> ).
    go_edit->set_textstream(
      EXPORTING
        text                   = gv_str    " Text as String with Carriage Returns and Linefeeds
      EXCEPTIONS
        error_cntl_call_method = 1
        not_supported_by_gui   = 2
        OTHERS                 = 3
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    IF gv_readonly EQ abap_true..
      go_edit->set_readonly_mode( 1 ).
    ELSE.
      go_edit->set_readonly_mode( 0 ).
    ENDIF.
  ENDIF.

ENDFORM.                    " LOAD_XML
FORM enqueue.
  IF p_edit EQ abap_false.
    gv_readonly = abap_true.
    EXIT.
  ENDIF.

  IF pr_wdcc EQ 'X'.
    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        object              = gs_wdy_config_key
        object_class        = 'WDCC'
        mode                = 'MODIFY'
        global_lock         = 'X'
        suppress_dialog     = ''
      EXCEPTIONS
        cancelled           = 1
        permission_failure  = 2
        unknown_objectclass = 3
        OTHERS              = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      gv_readonly = abap_true.
    ENDIF.
    IF gv_readonly = abap_false.
      CALL FUNCTION 'ENQUEUE_E_WDY_CONFCOMP'
        EXPORTING
          mode_wdy_config_data = 'E'
          config_id            = gs_wdy_config_key-config_id
          config_type          = gs_wdy_config_key-config_type
          config_var           = gs_wdy_config_key-config_var
          x_config_id          = 'X'
          x_config_type        = 'X'
          x_config_var         = 'X'
        EXCEPTIONS
          foreign_lock         = 1
          system_failure       = 2
          OTHERS               = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        gv_readonly = abap_true.
      ENDIF.
    ENDIF.
  ELSEIF pr_wdca EQ 'X'.
    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        object              = gs_wdy_config_key
        object_class        = 'WDCA'
        mode                = 'MODIFY'
        global_lock         = 'X'
        suppress_dialog     = ''
      EXCEPTIONS
        cancelled           = 1
        permission_failure  = 2
        unknown_objectclass = 3
        OTHERS              = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      gv_readonly = abap_true.
    ENDIF.
    IF gv_readonly = abap_false.
      CALL FUNCTION 'ENQUEUE_E_WDY_CONFAPPL'
        EXPORTING
          mode_wdy_config_appl = 'E'
          config_id            = gs_wdy_config_key-config_id
          config_type          = gs_wdy_config_key-config_type
          config_var           = gs_wdy_config_key-config_var
          x_config_id          = 'X'
          x_config_type        = 'X'
          x_config_var         = 'X'
        EXCEPTIONS
          foreign_lock         = 1
          system_failure       = 2
          OTHERS               = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        gv_readonly = abap_true.
      ENDIF.
    ENDIF.
  ELSEIF pr_enho EQ 'X'.
    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        object              = pa_enho
        object_class        = 'ENHO'
        mode                = 'MODIFY'
        global_lock         = 'X'
        suppress_dialog     = ''
      EXCEPTIONS
        cancelled           = 1
        permission_failure  = 2
        unknown_objectclass = 3
        OTHERS              = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      gv_readonly = abap_true.
    ENDIF.
    IF gv_readonly = abap_false.
      CALL FUNCTION 'ENQUEUE_E_ENHANCE'
        EXPORTING
          mode_eenhance  = 'E'
          object_class   = 'ENHO'
          enhname        = pa_enho
          x_object_class = 'X'
          x_enhname      = 'X'
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        gv_readonly = abap_true.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.
FORM dequeue.
  CHECK: gv_readonly = abap_false.
  IF pr_wdcc EQ 'X'.
    CALL FUNCTION 'DEQUEUE_E_WDY_CONFCOMP'
      EXPORTING
        mode_wdy_config_data = 'E'
        config_id            = gs_wdy_config_key-config_id
        config_type          = gs_wdy_config_key-config_type
        config_var           = gs_wdy_config_key-config_var
        x_config_id          = 'X'
        x_config_type        = 'X'
        x_config_var         = 'X'.
  ELSEIF pr_wdca EQ 'X'.
    CALL FUNCTION 'DEQUEUE_E_WDY_CONFAPPL'
      EXPORTING
        mode_wdy_config_appl = 'E'
        config_id            = gs_wdy_config_key-config_id
        config_type          = gs_wdy_config_key-config_type
        config_var           = gs_wdy_config_key-config_var
        x_config_id          = 'X'
        x_config_type        = 'X'
        x_config_var         = 'X'.
  ELSEIF pr_enho EQ 'X'.
    CALL FUNCTION 'DEQUEUE_E_ENHANCE'
      EXPORTING
        mode_eenhance  = 'E'
        object_class   = 'ENHO'
        enhname        = pa_enho
        x_object_class = space
        x_enhname      = space.
  ENDIF.
ENDFORM.
