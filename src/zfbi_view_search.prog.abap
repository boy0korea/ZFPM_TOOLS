* ZFPM_TOOLS

REPORT zfbi_view_search.

TABLES: wdy_md_change_information, /bofu/lfbiv_map, /bofu/s_fbiv_hdr.

**********************************************************************
* SCREEN
**********************************************************************
SELECT-OPTIONS: so_view FOR /bofu/lfbiv_map-view_name,
                so_cr_by FOR wdy_md_change_information-author,
                so_cr_on FOR wdy_md_change_information-createdon,
                so_ch_by FOR wdy_md_change_information-changedby,
                so_ch_on FOR wdy_md_change_information-changedon,
                so_bo FOR /bofu/s_fbiv_hdr-bo,
                so_node FOR /bofu/s_fbiv_hdr-node,
                so_ui_st FOR /bofu/s_fbiv_hdr-ui_structure,
                so_mapp FOR /bofu/s_fbiv_hdr-ui_mapper_cls,
                so_exit FOR /bofu/s_fbiv_hdr-ui_exit_intf_cls.

**********************************************************************
START-OF-SELECTION.
**********************************************************************
  PERFORM execute.



**********************************************************************
* global variable
**********************************************************************
  TYPES: BEGIN OF ts_outtab.
           INCLUDE TYPE /bofu/s_fbiv_hdr.
           INCLUDE TYPE wdy_md_change_information.
  TYPES:   style TYPE lvc_t_styl,
         END OF ts_outtab.
  DATA: gt_outtab         TYPE TABLE OF ts_outtab,
        gt_outtab_old     TYPE TABLE OF ts_outtab,
        gv_edit_mode      TYPE flag,
        gv_alv_first_time TYPE flag.

**********************************************************************
* form
**********************************************************************
FORM execute.
  DATA: lt_wdy_config_data TYPE TABLE OF wdy_config_data,
        ls_wdy_config_data TYPE wdy_config_data,
        lv_xstring         TYPE xstring,
        ls_outtab          TYPE ts_outtab,
        lt_fieldcat_lvc    TYPE lvc_t_fcat,
        ls_fieldcat_lvc    TYPE lvc_s_fcat,
        ls_layout_lvc      TYPE lvc_s_layo,
        lt_excluding       TYPE slis_t_extab,
        ls_exit_by_user    TYPE slis_exit_by_user.
  FIELD-SYMBOLS: <ls_fieldcat_lvc> TYPE lvc_s_fcat.

  AUTHORITY-CHECK OBJECT 'S_DEVELOP' ID 'ACTVT' FIELD '02'.
  IF sy-subrc EQ 0.
    gv_edit_mode = abap_true.
  ELSE.
    gv_edit_mode = abap_false.
    APPEND VALUE #( fcode = 'SAVE' ) TO lt_excluding.
  ENDIF.

*  SELECT view_name
*    INTO TABLE lt_view_name
*    FROM /bofu/lfbiv_map
*    WHERE view_name <> ''
*      AND view_name IN so_view
*      AND ui_structure IN so_ui_st.
*  SORT lt_view_name.

  SELECT *
    INTO TABLE lt_wdy_config_data
    FROM wdy_config_data
    WHERE component = /bofu/if_fbi_view_types=>gc_wd_config_component
      AND config_id IN so_view
      AND author IN so_cr_by
      AND createdon IN so_cr_on
      AND changedby IN so_ch_by
      AND changedon IN so_ch_on.
  SORT lt_wdy_config_data BY config_id.


  LOOP AT lt_wdy_config_data INTO ls_wdy_config_data.
    CLEAR: ls_outtab.

    ls_outtab-name = ls_wdy_config_data-config_id.
    ls_outtab-author = ls_wdy_config_data-author.
    ls_outtab-createdon = ls_wdy_config_data-createdon.
    ls_outtab-changedby = ls_wdy_config_data-changedby.
    ls_outtab-changedon = ls_wdy_config_data-changedon.

    PERFORM get_header USING ls_wdy_config_data-xcontent ls_outtab.

    IF so_bo IS NOT INITIAL.
      CHECK: ls_outtab-bo IN so_bo.
    ENDIF.
    IF so_node IS NOT INITIAL.
      CHECK: ls_outtab-node IN so_node.
    ENDIF.
    IF so_ui_st IS NOT INITIAL.
      CHECK: ls_outtab-ui_structure IN so_ui_st.
    ENDIF.
    IF so_mapp IS NOT INITIAL.
      CHECK: ls_outtab-ui_mapper_cls IN so_mapp.
    ENDIF.
    IF so_exit IS NOT INITIAL.
      CHECK: ls_outtab-ui_exit_intf_cls IN so_exit.
    ENDIF.

    APPEND ls_outtab TO gt_outtab.
  ENDLOOP.

  CHECK: gt_outtab IS NOT INITIAL.
  gt_outtab_old = gt_outtab.


  ls_layout_lvc-zebra = abap_true.
  ls_layout_lvc-no_rowmark = abap_true.
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = '/BOFU/S_FBIV_HDR'
    CHANGING
      ct_fieldcat      = lt_fieldcat_lvc.
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'WDY_MD_CHANGE_INFORMATION'
    CHANGING
      ct_fieldcat      = lt_fieldcat_lvc.
  LOOP AT lt_fieldcat_lvc ASSIGNING <ls_fieldcat_lvc>.
    <ls_fieldcat_lvc>-edit = gv_edit_mode.
    CASE <ls_fieldcat_lvc>-fieldname.
      WHEN 'NAME'.
        <ls_fieldcat_lvc>-edit = abap_false.
        <ls_fieldcat_lvc>-key = abap_true.
        <ls_fieldcat_lvc>-outputlen = 32.
      WHEN 'CREATED_BY'
        OR 'CREATED_ON'
        OR 'CHANGED_BY'
        OR 'CHANGED_ON'.
        " hide
        <ls_fieldcat_lvc>-tech = abap_true.
      WHEN 'AUTHOR'
        OR 'CREATEDON'
        OR 'CHANGEDBY'
        OR 'CHANGEDON'.
        <ls_fieldcat_lvc>-edit = abap_false.
      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.
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
      t_outtab                 = gt_outtab                   " Table with data to be displayed
    EXCEPTIONS
      program_error            = 1                          " Program Errors
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CLEAR: gt_outtab, gt_outtab_old.

ENDFORM.
FORM get_header USING iv_xstring TYPE xstring cs_header TYPE data.
  DATA: lt_expl      TYPE wdy_cfg_expl_data_tab,
        ls_expl_attr TYPE wdy_cfg_expl_attribute,
        lv_string    TYPE string,
        lv_offset    TYPE i,
        lv_offset2   TYPE i,
        lv_length    TYPE i.
  FIELD-SYMBOLS: <lv_data> TYPE data.

  lv_string = cl_wdr_configuration_utils=>xml_xstring2string( in_xstring = iv_xstring ).
  FIND '<Node Name="HEADER"' IN lv_string MATCH OFFSET lv_offset.
  CHECK: sy-subrc EQ 0.

  FIND '>' IN SECTION OFFSET lv_offset OF lv_string MATCH OFFSET lv_offset2.
  lv_offset2 = lv_offset2 - 1.
  IF lv_string+lv_offset2(1) EQ '/'.
    lv_offset2 = lv_offset2 + 2.
  ELSE.
    FIND '</Node>' IN SECTION OFFSET lv_offset2 OF lv_string MATCH OFFSET lv_offset2.
    lv_offset2 = lv_offset2 + 7.
  ENDIF.
  lv_length = lv_offset2 - lv_offset.

  lv_string = `<Component>` && lv_string+lv_offset(lv_length) && `</Component>`.


  CALL TRANSFORMATION wdr_cfg_comp_transl SOURCE XML lv_string RESULT table = lt_expl.
  LOOP AT lt_expl[ 2 ]-attr_list INTO ls_expl_attr.
    ASSIGN COMPONENT ls_expl_attr-name OF STRUCTURE cs_header TO <lv_data>.
    IF sy-subrc EQ 0.
      <lv_data> = ls_expl_attr-value.
    ENDIF.
  ENDLOOP.

ENDFORM.
FORM set_header USING cv_xstring TYPE xstring is_header TYPE data.
  DATA: lt_expl         TYPE wdy_cfg_expl_data_tab,
        ls_expl         TYPE wdy_cfg_expl_data,
        ls_expl_attr    TYPE wdy_cfg_expl_attribute,
        ls_header       TYPE /bofu/s_fbiv_hdr_data,
        lo_rtti         TYPE REF TO cl_abap_structdescr,
        ls_comp         TYPE abap_compdescr,
        lv_header_xml   TYPE string,
        lo_document     TYPE REF TO if_ixml_document,
        lo_root_element TYPE REF TO if_ixml_element,
        lv_string       TYPE string,
        lv_offset       TYPE i,
        lv_offset2      TYPE i,
        lv_length       TYPE i.
  FIELD-SYMBOLS: <lv_data> TYPE data.

  MOVE-CORRESPONDING is_header TO ls_header.
  lo_rtti ?= cl_abap_structdescr=>describe_by_data( ls_header ).

  CLEAR: ls_expl.
  ls_expl-simple_format = abap_true.
  ls_expl-path = '.HEADER'.
  ls_expl-parent_path = 'HEADER'.
  APPEND ls_expl TO lt_expl.

  CLEAR: ls_expl.
  ls_expl-simple_format = abap_true.
  ls_expl-path = '.HEADER.000001'.
  ls_expl-parent_path = '.HEADER'.
  ls_expl-node_name = 'HEADER'.
  ls_expl-index = '000001'.
  LOOP AT lo_rtti->components INTO ls_comp.
    ASSIGN COMPONENT ls_comp-name OF STRUCTURE ls_header TO <lv_data>.
    IF <lv_data> IS NOT INITIAL.
      ls_expl_attr-name = ls_comp-name.
      ls_expl_attr-value = <lv_data>.
      APPEND ls_expl_attr TO ls_expl-attr_list.
    ENDIF.
  ENDLOOP.
  APPEND ls_expl TO lt_expl.


  lo_document = cl_ixml=>create( )->create_document( ).
  lo_root_element = lo_document->create_element( name = if_wdr_cfg_constants=>c_xml_tag-e_component ).
  lo_document->append_child( lo_root_element ).
  cl_wdr_cfg_persistence_utils=>add_comp_expl_data_to_xml(
    EXPORTING
      expl_data_tab = lt_expl
      root          = lo_root_element
      document      = lo_document
  ).
  lv_header_xml = cl_wdr_configuration_utils=>xml_dom2string( in_dom = lo_document ).
*  CATCH cx_wd_configuration. " Exception Class for WD Configuration
  FIND '<Node Name="HEADER"' IN lv_header_xml MATCH OFFSET lv_offset.
  lv_header_xml = lv_header_xml+lv_offset.
  lv_length = strlen( lv_header_xml ) - 12.   " </Component>
  lv_header_xml = lv_header_xml(lv_length).


  lv_string = cl_wdr_configuration_utils=>xml_xstring2string( in_xstring = cv_xstring ).
  FIND '<Node Name="HEADER"' IN lv_string MATCH OFFSET lv_offset.
  CHECK: sy-subrc EQ 0.

  FIND '>' IN SECTION OFFSET lv_offset OF lv_string MATCH OFFSET lv_offset2.
  lv_offset2 = lv_offset2 - 1.
  IF lv_string+lv_offset2(1) EQ '/'.
    lv_offset2 = lv_offset2 + 2.
  ELSE.
    FIND '</Node>' IN SECTION OFFSET lv_offset2 OF lv_string MATCH OFFSET lv_offset2.
    lv_offset2 = lv_offset2 + 7.
  ENDIF.

  lv_string = lv_string(lv_offset)
           && lv_header_xml
           && lv_string+lv_offset2.

  cv_xstring = cl_wdr_configuration_utils=>xml_string2xstring( in_string = lv_string ).
*  cl_wdr_cfg_persistence_utils=>save_comp_config_to_db(
*    EXPORTING
*      config_data = ls_wdcc
*      translator  = lo_translator
*  ).

ENDFORM.

FORM edit_pf_status_set USING ps_extab TYPE slis_t_extab.
  SET PF-STATUS 'EDIT' EXCLUDING ps_extab.
ENDFORM.
FORM edit_user_command USING pv_ucomm ps_select TYPE slis_selfield.
  DATA: ls_outtab TYPE ts_outtab.

  CASE pv_ucomm.
    WHEN '&IC1'. "this is for double click on alv output.
*NAME 1 Types /BOFU/FBI_VIEW  CHAR  50  0 0 FBI View
*BO 1 Types /BOFU/BO  CHAR  30  0 0 Business Object
*NODE 1 Types /BOFU/NODE  CHAR  30  0 0 Node
*UI_STRUCTURE 1 Types /BOFU/FBI_NODE_UI_STRUCT  CHAR  30  0 0 Node UI Structure
*UI_MAPPER_CLS  1 Types /BOFU/FBI_NODE_UI_MAPPER  CHAR  30  0 0 UI Mapper Class for Business Object Node
*UI_EXIT_INTF_CLS 1 Types /BOFU/FBI_NODE_EXIT_INTF  CHAR  30  0 0 Exit Interface Class for Business Object Node
*TECH_FLD_STR 1 Types /BOFU/FBI_TECH_FLD_STRUCT CHAR  30  0 0 Technical Fields Structure for field control
*OUTPUT_STR 1 Types /BOFU/FBI_OUTPUT_STRUCT CHAR  30  0 0 Output Structure (Data+TechFields+RelatedView Structs)
      CASE ps_select-fieldname.
        WHEN 'NAME'.
          PERFORM do_web_cfg USING ps_select-value.
        WHEN 'UI_MAPPER_CLS'
          OR 'UI_EXIT_INTF_CLS'.
          PERFORM do_class USING ps_select-value.
        WHEN 'UI_STRUCTURE'
          OR 'TECH_FLD_STR'
          OR 'OUTPUT_STR'.
          PERFORM do_structure USING ps_select-value.
      ENDCASE.
    WHEN 'XML'.
      CHECK: ps_select-tabindex > 0.
      READ TABLE gt_outtab INTO ls_outtab INDEX ps_select-tabindex.
      PERFORM do_xml_wdcc USING ls_outtab-name.
    WHEN 'WEB_CFG'.
      CHECK: ps_select-tabindex > 0.
      READ TABLE gt_outtab INTO ls_outtab INDEX ps_select-tabindex.
      PERFORM do_web_cfg USING ls_outtab-name.
    WHEN 'SE80'.
      CHECK: ps_select-tabindex > 0.
      READ TABLE gt_outtab INTO ls_outtab INDEX ps_select-tabindex.
      PERFORM do_se80 USING ls_outtab-name.
    WHEN 'SAVE'.
      PERFORM flush_alv.
      PERFORM do_save_edit.
  ENDCASE.
ENDFORM.
FORM do_xml_wdcc USING iv_wdcc.
  SUBMIT zfpm_tools_xml_edit WITH pr_wdcc = abap_true WITH pa_wdcc = iv_wdcc WITH p_edit = gv_edit_mode AND RETURN.
ENDFORM.
FORM do_class USING iv_class.
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
FORM do_structure USING iv_structure.
  DATA: lv_operation TYPE seu_action.

  CHECK: iv_structure IS NOT INITIAL.
  IF gv_edit_mode EQ abap_true.
    lv_operation = 'EDIT'.
  ELSE.
    lv_operation = 'SHOW'.
  ENDIF.
  CALL FUNCTION 'RS_TOOL_ACCESS'
    EXPORTING
      operation           = lv_operation    " Operation
      object_name         = iv_structure    " Object Name
      object_type         = 'STRU'    " Object Type
    EXCEPTIONS
      not_executed        = 1
      invalid_object_type = 2
      OTHERS              = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.
FORM do_se80 USING iv_wdcc.
  DATA: lv_object_name    TYPE char40,
        ls_wdy_config_key TYPE wdy_config_key.

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
  DATA: lv_application TYPE wdy_application_name,
        lt_parameter   TYPE tihttpnvp,
        ls_parameter   TYPE ihttpnvp.

  lv_application = 'CONFIGURE_COMPONENT'.

  CLEAR: lt_parameter, ls_parameter.
  ls_parameter-name = 'COMP_NAME'.
  ls_parameter-value = /bofu/if_fbi_view_types=>gc_wd_config_component.
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
  ls_parameter-value = iv_wdcc.
  APPEND ls_parameter TO lt_parameter.
  CLEAR: ls_parameter.
  ls_parameter-name = 'CONFIG_TYPE'.
  ls_parameter-value = '00'.
  APPEND ls_parameter TO lt_parameter.
  CLEAR: ls_parameter.
  ls_parameter-name = 'CONFIG_VAR'.
  ls_parameter-value = ''.
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
  DATA: ls_outtab_old      TYPE ts_outtab,
        ls_outtab_new      TYPE ts_outtab,
        ls_wdy_config_data TYPE wdy_config_data,
        lv_index           TYPE i.

  IF gt_outtab EQ gt_outtab_old.
    " nothing to save.
    RETURN.
  ENDIF.

  LOOP AT gt_outtab INTO ls_outtab_new.
    lv_index = sy-tabix.
    READ TABLE gt_outtab_old INTO ls_outtab_old INDEX lv_index.
    IF ls_outtab_new <> ls_outtab_old.
      SELECT SINGLE *
        INTO ls_wdy_config_data
        FROM wdy_config_data
        WHERE config_id = ls_outtab_new-name
          AND config_type = '00'
          AND config_var = ''
      .
      PERFORM set_header USING ls_wdy_config_data-xcontent ls_outtab_new.
      ls_outtab_new-changed_by = sy-uname.
      GET TIME STAMP FIELD ls_outtab_new-changed_on.
      ls_outtab_new-changedby = sy-uname.
      ls_outtab_new-changedon = sy-datum.
      zcl_fpm_tools=>save_wdcc( is_wdcc = ls_wdy_config_data ).
    ENDIF.
  ENDLOOP.

  gt_outtab_old = gt_outtab.
ENDFORM.
FORM flush_alv.
  DATA: lo_grid TYPE REF TO cl_gui_alv_grid.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = lo_grid.

  CHECK: lo_grid IS NOT INITIAL.
  lo_grid->check_changed_data( ).

ENDFORM.
