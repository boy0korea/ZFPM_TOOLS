* ZFPM_TOOLS

REPORT zfpm_tools.

**********************************************************************
* SCREEN
**********************************************************************
PARAMETERS: pr_edit  TYPE flag RADIOBUTTON GROUP rd DEFAULT 'X' USER-COMMAND rd,
            pr_copy  TYPE flag RADIOBUTTON GROUP rd,
            pr_dele  TYPE flag RADIOBUTTON GROUP rd,
            pr_impo  TYPE flag RADIOBUTTON GROUP rd,
            pr_expo  TYPE flag RADIOBUTTON GROUP rd,
            pr_xmle  TYPE flag RADIOBUTTON GROUP rd,
            pr_fbiv  TYPE flag RADIOBUTTON GROUP rd,
            pr_fpmwb TYPE flag RADIOBUTTON GROUP rd.

SELECTION-SCREEN ULINE.
PARAMETERS: pa_wdca  TYPE wdy_config_appl-config_id MEMORY ID wdca MATCHCODE OBJECT wd_conf_appl MODIF ID 1,
            pa_wdca2 TYPE wdy_config_appl-config_id MATCHCODE OBJECT wd_conf_appl MODIF ID 2,
            pa_wclas TYPE flag MODIF ID 3,
            pa_file  TYPE string LOWER CASE MODIF ID 4,
            pa_copym TYPE char1 AS LISTBOX VISIBLE LENGTH 10 MODIF ID 5.

**********************************************************************
INITIALIZATION.
**********************************************************************
  PERFORM init.

**********************************************************************
AT SELECTION-SCREEN OUTPUT.
**********************************************************************
  PERFORM loop_screen.

**********************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_file.
**********************************************************************
  PERFORM f4_file.

**********************************************************************
START-OF-SELECTION.
**********************************************************************
  PERFORM execute.


**********************************************************************
* global variable
**********************************************************************
  DATA: gt_uibb_info      TYPE zcl_fpm_tools=>tt_uibb_info,
        gt_uibb_info_old  TYPE zcl_fpm_tools=>tt_uibb_info,
        gv_edit_mode      TYPE flag,
        gv_alv_first_time TYPE flag.

**********************************************************************
* form
**********************************************************************
FORM init.
  DATA: lt_values TYPE vrm_values,
        ls_values TYPE vrm_value.

  ls_values-text = ' '.
  ls_values-key = ' '.
  APPEND ls_values TO lt_values.
  ls_values-text = 'Copy'.
  ls_values-key = 'C'.
  APPEND ls_values TO lt_values.
  ls_values-text = 'Inherit'.
  ls_values-key = 'I'.
  APPEND ls_values TO lt_values.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'PA_COPYM'
      values = lt_values.

  pa_copym = 'C'.
ENDFORM.
FORM loop_screen.
  CASE abap_true.
    WHEN pr_edit.
      LOOP AT SCREEN.
        CHECK: screen-group1 IS NOT INITIAL.
        CASE screen-group1.
          WHEN 1.
            screen-active = 1.
          WHEN OTHERS.
            screen-active = 0.
        ENDCASE.
        MODIFY SCREEN.
      ENDLOOP.
    WHEN pr_copy.
      LOOP AT SCREEN.
        CHECK: screen-group1 IS NOT INITIAL.
        CASE screen-group1.
          WHEN 1 OR 2 OR 5.
            screen-active = 1.
          WHEN OTHERS.
            screen-active = 0.
        ENDCASE.
        MODIFY SCREEN.
      ENDLOOP.
    WHEN pr_dele.
      LOOP AT SCREEN.
        CHECK: screen-group1 IS NOT INITIAL.
        CASE screen-group1.
          WHEN 1 OR 3.
            screen-active = 1.
          WHEN OTHERS.
            screen-active = 0.
        ENDCASE.
        MODIFY SCREEN.
      ENDLOOP.
    WHEN pr_impo.
      LOOP AT SCREEN.
        CHECK: screen-group1 IS NOT INITIAL.
        CASE screen-group1.
          WHEN 4.
            screen-active = 1.
          WHEN OTHERS.
            screen-active = 0.
        ENDCASE.
        MODIFY SCREEN.
      ENDLOOP.
    WHEN pr_expo.
      LOOP AT SCREEN.
        CHECK: screen-group1 IS NOT INITIAL.
        CASE screen-group1.
          WHEN 1.
            screen-active = 1.
          WHEN OTHERS.
            screen-active = 0.
        ENDCASE.
        MODIFY SCREEN.
      ENDLOOP.
    WHEN pr_xmle
      OR pr_fbiv
      OR pr_fpmwb.
      LOOP AT SCREEN.
        CHECK: screen-group1 IS NOT INITIAL.
        screen-active = 0.
        MODIFY SCREEN.
      ENDLOOP.
  ENDCASE.
ENDFORM.

FORM f4_file.
  DATA: lt_df         TYPE TABLE OF dynpread,
        ls_df         TYPE dynpread,
        lt_file_table	TYPE filetable,
        ls_file_table TYPE file_table,
        lv_rc	        TYPE i.

  ls_df-fieldname = 'PA_FILE'.
  APPEND ls_df TO lt_df.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname     = sy-cprog
      dynumb     = sy-dynnr
    TABLES
      dynpfields = lt_df.
  READ TABLE lt_df INTO ls_df INDEX 1.
  pa_file = ls_df-fieldvalue.

  cl_gui_frontend_services=>file_open_dialog(
    EXPORTING
      default_extension       = 'zip' " Default Extension
      default_filename        = pa_file  " Default File Name
      file_filter             = 'ZIP files (*.zip)|*.zip|'       " File Extension Filter String
      multiselection          = abap_false    " Multiple selections poss.
    CHANGING
      file_table              = lt_file_table        " Table Holding Selected Files
      rc                      = lv_rc                " Return Code, Number of Files or -1 If Error Occurred
    EXCEPTIONS
      file_open_dialog_failed = 1                 " "Open File" dialog failed
      cntl_error              = 2                 " Control error
      error_no_gui            = 3                 " No GUI available
      not_supported_by_gui    = 4                 " GUI does not support this
      OTHERS                  = 5
  ).
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  READ TABLE lt_file_table INTO ls_file_table INDEX 1.
  pa_file = ls_file_table-filename.

ENDFORM.

FORM execute.

  CASE abap_true.
    WHEN pr_edit.
      PERFORM execute_edit.
    WHEN pr_copy.
      PERFORM execute_copy.
    WHEN pr_dele.
      PERFORM execute_delete.
    WHEN pr_impo.
      PERFORM execute_import.
    WHEN pr_expo.
      PERFORM execute_export.
    WHEN pr_xmle.
      CALL TRANSACTION 'ZFPM_TOOLS_XML_EDIT'.
    WHEN pr_fbiv.
      CALL TRANSACTION 'ZFBI_VIEW_SEARCH'.
    WHEN pr_fpmwb.
      CALL TRANSACTION 'FPM_WB'.
  ENDCASE.

ENDFORM.

FORM execute_edit.
  DATA: lt_uibb_info    TYPE zcl_fpm_tools=>tt_uibb_info,
        lt_fieldcat_lvc TYPE lvc_t_fcat,
        ls_layout_lvc   TYPE lvc_s_layo,
        lt_excluding    TYPE slis_t_extab,
        ls_exit_by_user TYPE slis_exit_by_user.

  AUTHORITY-CHECK OBJECT 'S_DEVELOP' ID 'ACTVT' FIELD '02'.
  IF sy-subrc EQ 0.
    gv_edit_mode = abap_true.
  ELSE.
    gv_edit_mode = abap_false.
    APPEND VALUE #( fcode = 'SAVE' ) TO lt_excluding.
    APPEND VALUE #( fcode = 'COPY_FC' ) TO lt_excluding.
    APPEND VALUE #( fcode = 'INHERIT_FC' ) TO lt_excluding.
  ENDIF.

  zcl_fpm_tools=>get_fpm_tree(
    EXPORTING
      iv_wdca              = pa_wdca
      iv_read_feeder_class = abap_true
      iv_read_feeder_param = abap_true
      iv_read_description  = abap_true
    IMPORTING
      et_uibb_info         = gt_uibb_info
  ).
  gt_uibb_info_old = gt_uibb_info.
  CHECK: gt_uibb_info IS NOT INITIAL.

  APPEND VALUE #( fcode = '&OUP' ) TO lt_excluding.
  APPEND VALUE #( fcode = '&ODN' ) TO lt_excluding.
  ls_layout_lvc-zebra = abap_true.
  ls_layout_lvc-no_rowmark = abap_true.
  lt_fieldcat_lvc = VALUE #(
    ( fieldname = 'CONFIG_ID' rollname = 'WDY_CONFIG_ID' outputlen = 32 )
    ( fieldname = 'CONFIG_DESCRIPTION' rollname = 'WDY_MD_DESCRIPTION' outputlen = 60 lowercase = abap_true edit = gv_edit_mode )
    ( fieldname = 'FEEDER_CLASS' rollname = 'FPMGB_FEEDER_CLASS' outputlen = 32 edit = gv_edit_mode )
    ( fieldname = 'FEEDER_CLASS_DESCRIPTION' rollname = 'WDY_MD_DESCRIPTION' outputlen = 60 lowercase = abap_true edit = gv_edit_mode )
    ( fieldname = 'FEEDER_PARAM' reptext = 'Parameter' outputlen = 200 edit = abap_false )
  ).
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
*     i_interface_check        = space                      " Interface consistency check log output
*     i_bypassing_buffer       = i_bypassing_buffer         " Skip All Buffers?
*     i_buffer_active          = i_buffer_active            " Buffer active
      i_callback_program       = sy-repid                      " Name of the calling program
      i_callback_pf_status_set = 'EDIT_PF_STATUS_SET'                      " Set EXIT routine to status
      i_callback_user_command  = 'EDIT_USER_COMMAND'                      " EXIT routine for command handling
*     i_callback_top_of_page   = space                      " EXIT routine for handling TOP-OF-PAGE
*     i_callback_html_top_of_page = space                      " EXIT routine for HTML TOP-OF-PAGE
*     i_callback_html_end_of_list = space                      " EXIT routine for HTML END-OF-LIST
*     i_structure_name         = i_structure_name           " Internal Output Table Structure Name
*     i_background_id          = i_background_id            " Object ID of wallpaper
*     i_grid_title             = i_grid_title               " Control title
*     i_grid_settings          = i_grid_settings            " Grid settings
      is_layout_lvc            = ls_layout_lvc              " List Layout Specifications
      it_fieldcat_lvc          = lt_fieldcat_lvc            " Field Catalog with Field Descriptions
      it_excluding             = lt_excluding               " Table of inactive function codes
*     it_special_groups_lvc    = it_special_groups_lvc      " Grouping fields for column selection
*     it_sort_lvc              = it_sort_lvc                " Sort criteria for first list display
*     it_filter_lvc            = it_filter_lvc              " Filter criteria for first list output
*     it_hyperlink             = it_hyperlink               " Hyperlinks
*     is_sel_hide              = is_sel_hide                " Selection information modification
*     i_default                = 'X'                        " Initial variant active/inactive logic
*     i_save                   = space                      " Variants Can be Saved
*     is_variant               = is_variant                 " Variant information
*     it_events                = it_events                  " Table of events to perform
*     it_event_exit            = it_event_exit              " Standard fcode exit requests table
*     is_print_lvc             = is_print_lvc               " Print information
*     is_reprep_id_lvc         = is_reprep_id_lvc           " Initialization key for Re/Re interface
*     i_screen_start_column    = 0                          " Coordinates for list in dialog box
*     i_screen_start_line      = 0                          " Coordinates for list in dialog box
*     i_screen_end_column      = 0                          " Coordinates for list in dialog box
*     i_screen_end_line        = 0                          " Coordinates for list in dialog box
*     i_html_height_top        = i_html_height_top
*     i_html_height_end        = i_html_height_end
*     it_alv_graphics          = it_alv_graphics
*     it_except_qinfo_lvc      = it_except_qinfo_lvc
*     ir_salv_fullscreen_adapter  = ir_salv_fullscreen_adapter
    IMPORTING
*     e_exit_caused_by_caller  = e_exit_caused_by_caller    " Delete list in CALLBACK_USER_COMMAND
      es_exit_caused_by_user   = ls_exit_by_user     " How the user left the list
    TABLES
      t_outtab                 = gt_uibb_info                   " Table with data to be displayed
    EXCEPTIONS
      program_error            = 1                          " Program Errors
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CLEAR: gt_uibb_info, gt_uibb_info_old.

ENDFORM.
FORM edit_pf_status_set USING ps_extab TYPE slis_t_extab.
  SET PF-STATUS 'EDIT' EXCLUDING ps_extab.
ENDFORM.
FORM edit_user_command USING pv_ucomm ps_select TYPE slis_selfield.
  DATA: ls_uibb_info TYPE zcl_fpm_tools=>ts_uibb_info.

  CASE pv_ucomm.
    WHEN '&IC1'. "this is for double click on alv output.
      CASE ps_select-fieldname.
        WHEN 'CONFIG_ID'.
          PERFORM do_web_cfg USING ps_select-value.
        WHEN 'FEEDER_CLASS'.
          PERFORM do_feeder USING ps_select-value.
      ENDCASE.
    WHEN 'FEEDER'.
      CHECK: ps_select-tabindex > 0.
      PERFORM flush_alv.
      READ TABLE gt_uibb_info INTO ls_uibb_info INDEX ps_select-tabindex.
      PERFORM do_feeder USING ls_uibb_info-feeder_class.
    WHEN 'COPY_FC'.
      CHECK: ps_select-tabindex > 0.
      PERFORM flush_alv.
      PERFORM do_copy_fc USING ps_select-tabindex.
      ps_select-refresh = abap_true.
    WHEN 'INHERIT_FC'.
      CHECK: ps_select-tabindex > 0.
      PERFORM flush_alv.
      PERFORM do_inherit_fc USING ps_select-tabindex.
      ps_select-refresh = abap_true.
    WHEN 'XML'.
      CHECK: ps_select-tabindex > 0.
      READ TABLE gt_uibb_info INTO ls_uibb_info INDEX ps_select-tabindex.
      PERFORM do_xml_wdcc USING ls_uibb_info-config_id.
    WHEN 'WEB_CFG'.
      CHECK: ps_select-tabindex > 0.
      READ TABLE gt_uibb_info INTO ls_uibb_info INDEX ps_select-tabindex.
      PERFORM do_web_cfg USING ls_uibb_info-config_id.
    WHEN 'SE80'.
      CHECK: ps_select-tabindex > 0.
      READ TABLE gt_uibb_info INTO ls_uibb_info INDEX ps_select-tabindex.
      PERFORM do_se80 USING ls_uibb_info-config_id.
    WHEN 'SAVE'.
      PERFORM flush_alv.
      PERFORM do_save_edit.
  ENDCASE.
ENDFORM.
FORM do_xml_wdcc USING iv_wdcc.
  SUBMIT zfpm_tools_xml_edit WITH pr_wdcc = abap_true WITH pa_wdcc = iv_wdcc WITH p_edit = gv_edit_mode AND RETURN.
ENDFORM.
FORM do_feeder USING iv_class.
  DATA: lv_operation TYPE seu_action.

  CHECK: iv_class IS NOT INITIAL.
  IF gv_edit_mode EQ abap_true.
    lv_operation = 'EDIT'.
  ELSE.
    lv_operation = 'SHOW'.
  ENDIF.
  CALL FUNCTION 'RS_TOOL_ACCESS'
    EXPORTING
      operation           = lv_operation    " Operation
      object_name         = iv_class    " Object Name
      object_type         = 'CLAS'    " Object Type
    EXCEPTIONS
      not_executed        = 1
      invalid_object_type = 2
      OTHERS              = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.
FORM do_copy_fc USING iv_index.
  DATA: ls_old_class TYPE seoclskey,
        ls_new_class TYPE seoclskey,
        lt_objects   TYPE TABLE OF dwinactiv,
        ls_objects   TYPE dwinactiv.

  ls_old_class-clsname = gt_uibb_info[ iv_index ]-feeder_class.
  CALL FUNCTION 'SEO_CLIF_COPY_INPUT_DYNPRO'
    EXPORTING
      old_clsname = ls_old_class-clsname
      clstype     = 0
    IMPORTING
      new_clsname = ls_new_class-clsname
    EXCEPTIONS
      cancelled   = 1           " Cancel
      OTHERS      = 2.

  CHECK: sy-subrc EQ 0.

  CALL FUNCTION 'SEO_CLASS_COPY'
    EXPORTING
      clskey       = ls_old_class
      new_clskey   = ls_new_class
    EXCEPTIONS
      not_existing = 1
      deleted      = 2
      is_interface = 3
      not_copied   = 4
      db_error     = 5
      no_access    = 6
      OTHERS       = 7.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    RETURN.
  ENDIF.

  ls_objects-obj_name = ls_new_class-clsname.
  ls_objects-object = 'CLAS'.
  APPEND ls_objects TO lt_objects.
  CALL FUNCTION 'RS_WORKING_OBJECTS_ACTIVATE'
    EXPORTING
      suppress_syntax_check  = abap_true
      suppress_corr_insert   = abap_true
      suppress_enqueue       = abap_true
    TABLES
      objects                = lt_objects
    EXCEPTIONS
      excecution_error       = 1
      cancelled              = 2
      insert_into_corr_error = 3
      OTHERS                 = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  zcl_fpm_tools=>set_feeder_class(
    EXPORTING
      iv_config_id        = gt_uibb_info[ iv_index ]-config_id
      iv_feeder_class     = ls_new_class-clsname
      iv_set_feeder_param = abap_false
  ).
  gt_uibb_info[ iv_index ]-feeder_class = ls_new_class-clsname.
  gt_uibb_info_old[ iv_index ]-feeder_class = ls_new_class-clsname.
ENDFORM.
FORM do_inherit_fc USING iv_index.
  DATA: ls_old_class   TYPE seoclskey,
        ls_new_class   TYPE seoclskey,
        ls_class       TYPE vseoclass,
        ls_inheritance TYPE vseoextend,
        lt_objects     TYPE TABLE OF dwinactiv,
        ls_objects     TYPE dwinactiv.

  ls_old_class-clsname = gt_uibb_info[ iv_index ]-feeder_class.
  zcl_fpm_tools=>can_inherit(
    EXPORTING
      iv_class       = ls_old_class-clsname
      iv_message     = abap_true
  ).

  CALL FUNCTION 'SEO_CLIF_COPY_INPUT_DYNPRO'
    EXPORTING
      old_clsname = ls_old_class-clsname
      clstype     = 0
    IMPORTING
      new_clsname = ls_new_class-clsname
    EXCEPTIONS
      cancelled   = 1           " Cancel
      OTHERS      = 2.

  CHECK: sy-subrc EQ 0.

  CLEAR: ls_class, ls_inheritance.
  ls_class-clsname = ls_new_class-clsname.
  ls_class-exposure = 2.
  ls_class-fixpt = abap_true.
  ls_class-unicode = abap_true.
  ls_inheritance-clsname = ls_class-clsname.
  ls_inheritance-refclsname = ls_old_class-clsname.
  ls_inheritance-state = 1.
  CALL FUNCTION 'SEO_CLASS_CREATE_COMPLETE'
    CHANGING
      class           = ls_class
      inheritance     = ls_inheritance
    EXCEPTIONS
      existing        = 1
      is_interface    = 2
      db_error        = 3
      component_error = 4
      no_access       = 5
      other           = 6
      OTHERS          = 7.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  ls_objects-obj_name = ls_new_class-clsname.
  ls_objects-object = 'CLAS'.
  APPEND ls_objects TO lt_objects.
  CALL FUNCTION 'RS_WORKING_OBJECTS_ACTIVATE'
    EXPORTING
      suppress_syntax_check  = abap_true
      suppress_corr_insert   = abap_true
      suppress_enqueue       = abap_true
    TABLES
      objects                = lt_objects
    EXCEPTIONS
      excecution_error       = 1
      cancelled              = 2
      insert_into_corr_error = 3
      OTHERS                 = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  zcl_fpm_tools=>set_feeder_class(
    EXPORTING
      iv_config_id        = gt_uibb_info[ iv_index ]-config_id
      iv_feeder_class     = ls_new_class-clsname
      iv_set_feeder_param = abap_false
  ).
  gt_uibb_info[ iv_index ]-feeder_class = ls_new_class-clsname.
  gt_uibb_info_old[ iv_index ]-feeder_class = ls_new_class-clsname.
ENDFORM.
FORM do_se80 USING iv_wdcc.
  DATA: lv_object_name    TYPE char40,
        ls_wdy_config_key TYPE wdy_config_key.

  CHECK: iv_wdcc IS NOT INITIAL.
  ls_wdy_config_key-config_id = iv_wdcc.
  lv_object_name = ls_wdy_config_key.

  CALL FUNCTION 'RS_TOOL_ACCESS'
    EXPORTING
      operation           = 'SHOW'    " Operation
      object_name         = lv_object_name    " Object Name
      object_type         = 'WDCC'    " Object Type
    EXCEPTIONS
      not_executed        = 1
      invalid_object_type = 2
      OTHERS              = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " DO_SE80
FORM do_web_cfg USING iv_wdcc.
  DATA: lv_application     TYPE wdy_application_name,
        lt_parameter       TYPE tihttpnvp,
        ls_parameter       TYPE ihttpnvp,
        ls_wdy_config_data TYPE wdy_config_data.

  CHECK: iv_wdcc IS NOT INITIAL.

  SELECT SINGLE *
    INTO ls_wdy_config_data
    FROM wdy_config_data
    WHERE config_id = iv_wdcc
      AND config_type = '00'
      AND config_var = ''.
  CHECK: sy-subrc EQ 0.

  lv_application = 'CONFIGURE_COMPONENT'.

  CLEAR: lt_parameter, ls_parameter.
  ls_parameter-name = 'COMP_NAME'.
  ls_parameter-value = ls_wdy_config_data-component.
  APPEND ls_parameter TO lt_parameter.
  CLEAR: ls_parameter.
  ls_parameter-name = 'ACTION'.
  IF gv_edit_mode EQ abap_true.
    ls_parameter-value = 'CHANGE'.
  ELSE.
    ls_parameter-value = 'DISPLAY'.
  ENDIF.
  APPEND ls_parameter TO lt_parameter.

  CLEAR: ls_parameter.
  ls_parameter-name = 'CONFIG_ID'.
  ls_parameter-value = ls_wdy_config_data-config_id.
  APPEND ls_parameter TO lt_parameter.
  CLEAR: ls_parameter.
  ls_parameter-name = 'CONFIG_TYPE'.
  ls_parameter-value = ls_wdy_config_data-config_type.
  APPEND ls_parameter TO lt_parameter.
  CLEAR: ls_parameter.
  ls_parameter-name = 'CONFIG_VAR'.
  ls_parameter-value = ls_wdy_config_data-config_var.
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
FORM do_save_edit.
  DATA: ls_uibb_info_old TYPE zcl_fpm_tools=>ts_uibb_info,
        ls_uibb_info_new TYPE zcl_fpm_tools=>ts_uibb_info,
        lv_index         TYPE i.

  IF gt_uibb_info EQ gt_uibb_info_old.
    " nothing to save.
    RETURN.
  ENDIF.

  LOOP AT gt_uibb_info INTO ls_uibb_info_new.
    lv_index = sy-tabix.
    READ TABLE gt_uibb_info_old INTO ls_uibb_info_old INDEX lv_index.
    IF ls_uibb_info_new-config_description <> ls_uibb_info_old-config_description.
      zcl_fpm_tools=>set_description(
        EXPORTING
          iv_name        = ls_uibb_info_new-config_id
          iv_typekind    = 'WDCC'
          iv_description = ls_uibb_info_new-config_description
      ).
    ENDIF.
    IF ls_uibb_info_new-feeder_class <> ls_uibb_info_old-feeder_class OR
       ls_uibb_info_new-feeder_param <> ls_uibb_info_old-feeder_param.
      zcl_fpm_tools=>set_feeder_class(
        EXPORTING
          iv_config_id        = ls_uibb_info_new-config_id
          iv_feeder_class     = ls_uibb_info_new-feeder_class
          iv_feeder_param     = ls_uibb_info_new-feeder_param
          iv_set_feeder_param = boolc( ls_uibb_info_new-feeder_param <> ls_uibb_info_old-feeder_param )
      ).
    ENDIF.
    IF ls_uibb_info_new-feeder_class_description <> ls_uibb_info_old-feeder_class_description.
      zcl_fpm_tools=>set_description(
        EXPORTING
          iv_name        = ls_uibb_info_new-feeder_class
          iv_typekind    = 'CLAS'
          iv_description = ls_uibb_info_new-feeder_class_description
      ).
    ENDIF.
  ENDLOOP.

  gt_uibb_info_old = gt_uibb_info.
ENDFORM.

FORM execute_copy.
  TYPES: BEGIN OF ts_outtab.
           INCLUDE TYPE zcl_fpm_tools_copy=>ts_uibb_name_map.
  TYPES:   mode_copy    TYPE c,
           mode_inherit TYPE c,
           style        TYPE lvc_t_styl,
         END OF ts_outtab.
  DATA: ls_fpm_name_map  TYPE zcl_fpm_tools_copy=>ts_fpm_name_map,
        ls_replace_rule  TYPE zcl_fpm_tools_copy=>ts_replace_rule,
        ls_uibb_name_map TYPE zcl_fpm_tools_copy=>ts_uibb_name_map,
        lt_uibb_name_map TYPE zcl_fpm_tools_copy=>tt_uibb_name_map,
        ls_outtab        TYPE ts_outtab,
        lt_outtab        TYPE TABLE OF ts_outtab,
        ls_style         TYPE lvc_s_styl,
        lt_fieldcat_lvc  TYPE lvc_t_fcat,
        ls_layout_lvc    TYPE lvc_s_layo,
        ls_exit_by_user  TYPE slis_exit_by_user.
  FIELD-SYMBOLS: <ls_outtab> TYPE ts_outtab.

  zcl_fpm_tools_copy=>step1(
    EXPORTING
      iv_from         = pa_wdca
      iv_to           = pa_wdca2
    IMPORTING
      es_fpm_name_map = ls_fpm_name_map
      es_replace_rule = ls_replace_rule
  ).
  CHECK: ls_fpm_name_map IS NOT INITIAL.

  zcl_fpm_tools_copy=>step2(
    EXPORTING
      iv_from          = pa_wdca
      is_replace_rule  = ls_replace_rule
      iv_feeder_class_copy_mode = pa_copym
    IMPORTING
      et_uibb_name_map = lt_uibb_name_map
  ).


  MOVE-CORRESPONDING lt_uibb_name_map TO lt_outtab.
  LOOP AT lt_outtab ASSIGNING <ls_outtab>.
    IF <ls_outtab>-feeder_class IS INITIAL.
      ls_style-fieldname = 'MODE_COPY'.
      ls_style-style = cl_gui_alv_grid=>mc_style_disabled.
      INSERT ls_style INTO TABLE <ls_outtab>-style.
      ls_style-fieldname = 'MODE_INHERIT'.
      ls_style-style = cl_gui_alv_grid=>mc_style_disabled.
      INSERT ls_style INTO TABLE <ls_outtab>-style.
    ELSEIF zcl_fpm_tools=>can_inherit( <ls_outtab>-feeder_class ) EQ abap_false.
      ls_style-fieldname = 'MODE_INHERIT'.
      ls_style-style = cl_gui_alv_grid=>mc_style_disabled.
      INSERT ls_style INTO TABLE <ls_outtab>-style.
      IF <ls_outtab>-feeder_class_copy_mode EQ 'I'.
        <ls_outtab>-feeder_class_copy_mode = 'C'.
      ENDIF.
    ENDIF.
    CASE <ls_outtab>-feeder_class_copy_mode.
      WHEN 'C'.
        <ls_outtab>-mode_copy = abap_true.
      WHEN 'I'.
        <ls_outtab>-mode_inherit = abap_true.
      WHEN OTHERS.
        ls_style-fieldname = 'FEEDER_CLASS_COPY'.
        ls_style-style = cl_gui_alv_grid=>mc_style_disabled.
        INSERT ls_style INTO TABLE <ls_outtab>-style.
        ls_style-fieldname = 'FEEDER_CLASS_DESCRIPTION_COPY'.
        ls_style-style = cl_gui_alv_grid=>mc_style_disabled.
        INSERT ls_style INTO TABLE <ls_outtab>-style.
    ENDCASE.
  ENDLOOP.

  CLEAR: ls_outtab.
  ls_outtab-config_id = ls_fpm_name_map-config_id.
  ls_outtab-config_id_copy = ls_fpm_name_map-config_id_copy.
  ls_outtab-config_description_copy = ls_fpm_name_map-config_description_copy.
  ls_outtab-feeder_class = ls_fpm_name_map-application.
  ls_outtab-feeder_class_copy = ls_fpm_name_map-application_copy.
  ls_outtab-feeder_class_copy_mode = 'C'.
  ls_outtab-feeder_class_description_copy = ls_fpm_name_map-application_description_copy.
  ls_outtab-mode_copy = abap_true.
  ls_outtab-style = VALUE #( ( fieldname = 'MODE_INHERIT' style = cl_gui_alv_grid=>mc_style_disabled ) ).
  INSERT ls_outtab INTO lt_outtab INDEX 1.

  ls_layout_lvc-zebra = abap_true.
  ls_layout_lvc-no_rowmark = abap_true.
*  ls_layout_lvc-edit = abap_true.
  ls_layout_lvc-stylefname = 'STYLE'.
*    TYPES:
*      BEGIN OF ts_uibb_name_map,
*        config_id                     TYPE wdy_config_id,
*        feeder_class                  TYPE fpmgb_feeder_class,
*        config_id_copy                TYPE wdy_config_id,
*        config_description_copy       TYPE wdy_md_description,
*        feeder_class_copy_mode        TYPE char1,
*        feeder_class_copy             TYPE fpmgb_feeder_class,
*        feeder_class_description_copy TYPE wdy_md_description,
*      END OF ts_uibb_name_map .
  lt_fieldcat_lvc = VALUE #(
    ( fieldname = 'CONFIG_ID' rollname = 'WDY_CONFIG_ID' outputlen = 32 )
    ( fieldname = 'CONFIG_ID_COPY' reptext = 'copy to' outputlen = 32 edit = abap_true )
    ( fieldname = 'CONFIG_DESCRIPTION_COPY' rollname = 'WDY_MD_DESCRIPTION' outputlen = 60 lowercase = abap_true edit = abap_true )
    ( fieldname = 'FEEDER_CLASS' rollname = 'FPMGB_FEEDER_CLASS' outputlen = 30 )
*    ( fieldname = 'FEEDER_CLASS_COPY_MODE' reptext = 'copy mode' drdn_hndl = 1 edit = abap_true )
    ( fieldname = 'MODE_COPY' reptext = 'copy' checkbox = abap_true edit = abap_true )
    ( fieldname = 'MODE_INHERIT' reptext = 'inherit' checkbox = abap_true edit = abap_true )
    ( fieldname = 'FEEDER_CLASS_COPY' reptext = 'copy to' outputlen = 30 edit = abap_true )
    ( fieldname = 'FEEDER_CLASS_DESCRIPTION_COPY' rollname = 'WDY_MD_DESCRIPTION' outputlen = 60 lowercase = abap_true edit = abap_true )
  ).
  gv_alv_first_time = abap_true.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
*     i_interface_check        = space                      " Interface consistency check log output
*     i_bypassing_buffer       = i_bypassing_buffer         " Skip All Buffers?
*     i_buffer_active          = i_buffer_active            " Buffer active
      i_callback_program       = sy-repid                      " Name of the calling program
      i_callback_pf_status_set = 'COPY_PF_STATUS_SET'                      " Set EXIT routine to status
      i_callback_user_command  = 'COPY_USER_COMMAND'                      " EXIT routine for command handling
*     i_callback_top_of_page   = space                      " EXIT routine for handling TOP-OF-PAGE
*     i_callback_html_top_of_page = space                      " EXIT routine for HTML TOP-OF-PAGE
*     i_callback_html_end_of_list = space                      " EXIT routine for HTML END-OF-LIST
*     i_structure_name         = i_structure_name           " Internal Output Table Structure Name
*     i_background_id          = i_background_id            " Object ID of wallpaper
*     i_grid_title             = i_grid_title               " Control title
*     i_grid_settings          = VALUE lvc_s_glay(  )            " Grid settings
      is_layout_lvc            = ls_layout_lvc              " List Layout Specifications
      it_fieldcat_lvc          = lt_fieldcat_lvc            " Field Catalog with Field Descriptions
      it_excluding             = VALUE slis_t_extab( ( fcode = '&OUP' ) ( fcode = '&ODN' ) )               " Table of inactive function codes
*     it_special_groups_lvc    = it_special_groups_lvc      " Grouping fields for column selection
*     it_sort_lvc              = it_sort_lvc                " Sort criteria for first list display
*     it_filter_lvc            = it_filter_lvc              " Filter criteria for first list output
*     it_hyperlink             = it_hyperlink               " Hyperlinks
*     is_sel_hide              = is_sel_hide                " Selection information modification
*     i_default                = 'X'                        " Initial variant active/inactive logic
*     i_save                   = space                      " Variants Can be Saved
*     is_variant               = is_variant                 " Variant information
      it_events                = VALUE slis_t_event( ( name = 'DATA_CHANGED' form = 'COPY_DATA_CHANGED' ) )                  " Table of events to perform
*     it_event_exit            = it_event_exit              " Standard fcode exit requests table
*     is_print_lvc             = is_print_lvc               " Print information
*     is_reprep_id_lvc         = is_reprep_id_lvc           " Initialization key for Re/Re interface
*     i_screen_start_column    = 0                          " Coordinates for list in dialog box
*     i_screen_start_line      = 0                          " Coordinates for list in dialog box
*     i_screen_end_column      = 0                          " Coordinates for list in dialog box
*     i_screen_end_line        = 0                          " Coordinates for list in dialog box
*     i_html_height_top        = i_html_height_top
*     i_html_height_end        = i_html_height_end
*     it_alv_graphics          = it_alv_graphics
*     it_except_qinfo_lvc      = it_except_qinfo_lvc
*     ir_salv_fullscreen_adapter  = ir_salv_fullscreen_adapter
    IMPORTING
*     e_exit_caused_by_caller  = e_exit_caused_by_caller    " Delete list in CALLBACK_USER_COMMAND
      es_exit_caused_by_user   = ls_exit_by_user     " How the user left the list
    TABLES
      t_outtab                 = lt_outtab                   " Table with data to be displayed
    EXCEPTIONS
      program_error            = 1                          " Program Errors
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CHECK: ls_exit_by_user IS INITIAL.

  READ TABLE lt_outtab INTO ls_outtab INDEX 1.
  DELETE lt_outtab INDEX 1.
  ls_fpm_name_map-config_id_copy = ls_outtab-config_id_copy.
  IF ls_outtab-mode_copy EQ abap_true.
    ls_fpm_name_map-application_copy = ls_outtab-feeder_class_copy.
    ls_fpm_name_map-application_description_copy = ls_outtab-feeder_class_description_copy.
  ELSE.
    CLEAR: ls_fpm_name_map-application_copy, ls_fpm_name_map-application_description_copy.
  ENDIF.
*  MOVE-CORRESPONDING lt_outtab TO lt_uibb_name_map.
  CLEAR: lt_uibb_name_map.
  LOOP AT lt_outtab INTO ls_outtab.
    IF ls_outtab-mode_copy EQ abap_true.
      ls_outtab-feeder_class_copy_mode = 'C'.
    ELSEIF ls_outtab-mode_inherit EQ abap_true.
      ls_outtab-feeder_class_copy_mode = 'I'.
    ELSE.
      CLEAR: ls_outtab-feeder_class_copy_mode.
    ENDIF.
    IF ls_outtab-feeder_class_copy IS INITIAL.
      CLEAR: ls_outtab-feeder_class_copy_mode.
    ENDIF.
    MOVE-CORRESPONDING ls_outtab TO ls_uibb_name_map.
    APPEND ls_uibb_name_map TO lt_uibb_name_map.
  ENDLOOP.

  zcl_fpm_tools_copy=>step3(
    EXPORTING
      is_fpm_name_map  = ls_fpm_name_map
      it_uibb_name_map = lt_uibb_name_map
  ).
ENDFORM.
FORM copy_pf_status_set USING ps_extab TYPE slis_t_extab.
  DATA: lo_grid      TYPE REF TO cl_gui_alv_grid,
        lt_drop_down TYPE lvc_t_drop,
        ls_drop_down TYPE lvc_s_drop.

  SET PF-STATUS 'COPY' EXCLUDING ps_extab.

  IF gv_alv_first_time EQ abap_true.
    gv_alv_first_time = abap_false.
    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        e_grid = lo_grid.
    lo_grid->register_edit_event( cl_gui_alv_grid=>mc_evt_modified ).

*    ls_drop_down-handle = 1.
*
*    ls_drop_down-value = ' '.
*    APPEND ls_drop_down TO lt_drop_down.
*    ls_drop_down-value = 'C'.
*    APPEND ls_drop_down TO lt_drop_down.
*    ls_drop_down-value = 'I'.
*    APPEND ls_drop_down TO lt_drop_down.
*
*    lo_grid->set_drop_down_table(
*      EXPORTING
*        it_drop_down = lt_drop_down
*    ).
*    lo_grid->refresh_table_display( ).
  ENDIF.
ENDFORM.
FORM copy_data_changed USING pi_data_changed TYPE REF TO cl_alv_changed_data_protocol.
  DATA : ls_mod_cells TYPE lvc_s_modi,
         lv_value     TYPE lvc_value.

  LOOP AT pi_data_changed->mt_good_cells INTO ls_mod_cells WHERE fieldname = 'MODE_COPY' OR fieldname = 'MODE_INHERIT'.
    IF ls_mod_cells-value IS NOT INITIAL.
      IF ls_mod_cells-fieldname EQ 'MODE_COPY'.
        pi_data_changed->modify_cell(
          EXPORTING
            i_row_id    = ls_mod_cells-row_id
            i_fieldname = 'MODE_INHERIT'
            i_value     = space
        ).
      ELSE.
        pi_data_changed->modify_cell(
          EXPORTING
            i_row_id    = ls_mod_cells-row_id
            i_fieldname = 'MODE_COPY'
            i_value     = space
        ).
      ENDIF.
      pi_data_changed->modify_style(
        EXPORTING
          i_row_id    = ls_mod_cells-row_id
          i_fieldname = 'FEEDER_CLASS_COPY'
          i_style     = cl_gui_alv_grid=>mc_style_enabled
      ).
      pi_data_changed->modify_style(
        EXPORTING
          i_row_id    = ls_mod_cells-row_id
          i_fieldname = 'FEEDER_CLASS_DESCRIPTION_COPY'
          i_style     = cl_gui_alv_grid=>mc_style_enabled
      ).
    ELSE.
      IF ls_mod_cells-fieldname EQ 'MODE_COPY'.
        pi_data_changed->get_cell_value(
          EXPORTING
            i_row_id    = ls_mod_cells-row_id
            i_fieldname = 'MODE_INHERIT'
          IMPORTING
            e_value     = lv_value
        ).
      ELSE.
        pi_data_changed->get_cell_value(
          EXPORTING
            i_row_id    = ls_mod_cells-row_id
            i_fieldname = 'MODE_COPY'
          IMPORTING
            e_value     = lv_value
        ).
      ENDIF.
      IF lv_value IS INITIAL.
        pi_data_changed->modify_cell(
          EXPORTING
            i_row_id    = ls_mod_cells-row_id
            i_fieldname = 'FEEDER_CLASS_COPY'
            i_value     = space
        ).
        pi_data_changed->modify_cell(
          EXPORTING
            i_row_id    = ls_mod_cells-row_id
            i_fieldname = 'FEEDER_CLASS_DESCRIPTION_COPY'
            i_value     = space
        ).
        pi_data_changed->modify_style(
          EXPORTING
            i_row_id    = ls_mod_cells-row_id
            i_fieldname = 'FEEDER_CLASS_COPY'
            i_style     = cl_gui_alv_grid=>mc_style_disabled
        ).
        pi_data_changed->modify_style(
          EXPORTING
            i_row_id    = ls_mod_cells-row_id
            i_fieldname = 'FEEDER_CLASS_DESCRIPTION_COPY'
            i_style     = cl_gui_alv_grid=>mc_style_disabled
        ).
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    "ALV_DATA_CHANGED
FORM copy_user_command USING pv_ucomm ps_select TYPE slis_selfield.
  CASE pv_ucomm.
    WHEN 'EXEC'.
      PERFORM flush_alv.
      ps_select-exit = abap_true.
  ENDCASE.
ENDFORM.

FORM execute_delete.

  zcl_fpm_tools=>delete_fpm_tree(
    EXPORTING
      iv_wdca              = pa_wdca
      iv_with_feeder_class = pa_wclas
  ).

ENDFORM.

FORM execute_import.
  DATA: lv_filelength TYPE i,
        lt_temptable  TYPE w3mimetabtype,
        lv_xstring    TYPE xstring.

  CHECK: pa_file IS NOT INITIAL.

  cl_gui_frontend_services=>gui_upload(
    EXPORTING
      filename                = pa_file              " Name of file
      filetype                = 'BIN'              " File Type (ASCII, Binary)
    IMPORTING
      filelength              = lv_filelength         " File Length
    CHANGING
      data_tab                = lt_temptable           " Transfer table for file contents
    EXCEPTIONS
      file_open_error         = 1                  " File does not exist and cannot be opened
      file_read_error         = 2                  " Error when reading file
      no_batch                = 3                  " Cannot execute front-end function in background
      gui_refuse_filetransfer = 4                  " Incorrect front end or error on front end
      invalid_type            = 5                  " Incorrect parameter FILETYPE
      no_authority            = 6                  " No upload authorization
      unknown_error           = 7                  " Unknown error
      bad_data_format         = 8                  " Cannot Interpret Data in File
      header_not_allowed      = 9                  " Invalid header
      separator_not_allowed   = 10                 " Invalid separator
      header_too_long         = 11                 " Header information currently restricted to 1023 bytes
      unknown_dp_error        = 12                 " Error when calling data provider
      access_denied           = 13                 " Access to file denied.
      dp_out_of_memory        = 14                 " Not enough memory in data provider
      disk_full               = 15                 " Storage medium is full.
      dp_timeout              = 16                 " Data provider timeout
      not_supported_by_gui    = 17                 " GUI does not support this
      error_no_gui            = 18                 " GUI not available
      OTHERS                  = 19
  ).
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
    EXPORTING
      input_length = lv_filelength
    IMPORTING
      buffer       = lv_xstring
    TABLES
      binary_tab   = lt_temptable
    EXCEPTIONS
      failed       = 1
      OTHERS       = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  zcl_fpm_tools=>import_fpm_tree( iv_zip = lv_xstring ).
ENDFORM.

FORM execute_export.
  DATA: lv_xstring    TYPE xstring,
        lv_filename   TYPE string,
        lv_path       TYPE string,
        lv_fullpath   TYPE string,
        lv_filelength TYPE i,
        lt_temptable  TYPE w3mimetabtype.

  lv_filename = pa_wdca && '.' && sy-datum && '.FPM.zip'.
  IF lv_filename(1) EQ '/'.
    REPLACE ALL OCCURRENCES OF '/' IN lv_filename WITH '#'.
  ENDIF.
  cl_gui_frontend_services=>file_save_dialog(
    EXPORTING
*      window_title              = window_title      " Window Title
      default_extension         = 'zip' " Default Extension
      default_file_name         = lv_filename " Default File Name
*      with_encoding             = with_encoding
      file_filter               = 'ZIP files (*.zip)|*.zip|'       " File Type Filter Table
*      initial_directory         = initial_directory " Initial Directory
*      prompt_on_overwrite       = 'X'
    CHANGING
      filename                  = lv_filename          " File Name to Save
      path                      = lv_path              " Path to File
      fullpath                  = lv_fullpath          " Path + File Name
*      user_action               = user_action       " User Action (C Class Const ACTION_OK, ACTION_OVERWRITE etc)
*      file_encoding             = file_encoding
    EXCEPTIONS
      cntl_error                = 1                 " Control error
      error_no_gui              = 2                 " No GUI available
      not_supported_by_gui      = 3                 " GUI does not support this
      invalid_default_file_name = 4                 " Invalid default file name
      OTHERS                    = 5
  ).
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  CHECK: lv_fullpath IS NOT INITIAL.

  lv_xstring = zcl_fpm_tools=>export_fpm_tree( iv_wdca = pa_wdca ).



  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      buffer        = lv_xstring
    IMPORTING
      output_length = lv_filelength
    TABLES
      binary_tab    = lt_temptable.
  cl_gui_frontend_services=>gui_download(
    EXPORTING
      bin_filesize              = lv_filelength         " File length for binary files
      filename                  = lv_fullpath             " Name of file
      filetype                  = 'BIN'                " File type (ASCII, binary ...)
    CHANGING
      data_tab                  = lt_temptable             " Transfer table
    EXCEPTIONS
      file_write_error          = 1                    " Cannot write to file
      no_batch                  = 2                    " Cannot execute front-end function in background
      gui_refuse_filetransfer   = 3                    " Incorrect Front End
      invalid_type              = 4                    " Invalid value for parameter FILETYPE
      no_authority              = 5                    " No Download Authorization
      unknown_error             = 6                    " Unknown error
      header_not_allowed        = 7                    " Invalid header
      separator_not_allowed     = 8                    " Invalid separator
      filesize_not_allowed      = 9                    " Invalid file size
      header_too_long           = 10                   " Header information currently restricted to 1023 bytes
      dp_error_create           = 11                   " Cannot create DataProvider
      dp_error_send             = 12                   " Error Sending Data with DataProvider
      dp_error_write            = 13                   " Error Writing Data with DataProvider
      unknown_dp_error          = 14                   " Error when calling data provider
      access_denied             = 15                   " Access to file denied.
      dp_out_of_memory          = 16                   " Not enough memory in data provider
      disk_full                 = 17                   " Storage medium is full.
      dp_timeout                = 18                   " Data provider timeout
      file_not_found            = 19                   " Could not find file
      dataprovider_exception    = 20                   " General Exception Error in DataProvider
      control_flush_error       = 21                   " Error in Control Framework
      not_supported_by_gui      = 22                   " GUI does not support this
      error_no_gui              = 23                   " GUI not available
      OTHERS                    = 24
  ).
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM.
FORM flush_alv.
  DATA: lo_grid TYPE REF TO cl_gui_alv_grid.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = lo_grid.

  CHECK: lo_grid IS NOT INITIAL.
  lo_grid->check_changed_data( ).

ENDFORM.
