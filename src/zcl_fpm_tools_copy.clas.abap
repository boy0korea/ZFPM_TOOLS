CLASS zcl_fpm_tools_copy DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ts_replace_rule,
        ns_to    TYPE string,
        beg_from TYPE string,
        beg_to   TYPE string,
        end_from TYPE string,
        end_to   TYPE string,
      END OF ts_replace_rule .
    TYPES:
      BEGIN OF ts_fpm_name_map,
        application                  TYPE wdy_config_appl-application,
        config_id                    TYPE wdy_config_appl-config_id,
        application_copy             TYPE wdy_config_appl-application,
        config_id_copy               TYPE wdy_config_appl-config_id,
        application_description_copy TYPE wdy_md_description,
        config_description_copy      TYPE wdy_md_description,
      END OF ts_fpm_name_map .
    TYPES:
      BEGIN OF ts_uibb_name_map,
        config_id                     TYPE wdy_config_id,
        feeder_class                  TYPE fpmgb_feeder_class,
        config_id_copy                TYPE wdy_config_id,
        config_description_copy       TYPE wdy_md_description,
        feeder_class_copy_mode        TYPE char1,
        feeder_class_copy             TYPE fpmgb_feeder_class,
        feeder_class_description_copy TYPE wdy_md_description,
      END OF ts_uibb_name_map .
    TYPES:
      tt_uibb_name_map TYPE TABLE OF ts_uibb_name_map .

    CLASS-METHODS execute
      IMPORTING
        !iv_from     TYPE wdy_config_id
        !iv_to       TYPE wdy_config_id
        !iv_devclass TYPE devclass OPTIONAL
        !iv_trkorr   TYPE trkorr OPTIONAL .
    CLASS-METHODS get_lcs
      IMPORTING
        !iv_x        TYPE clike
        !iv_y        TYPE clike
      EXPORTING
        !ev_length   TYPE i
        !ev_x_offset TYPE i
        !ev_y_offset TYPE i .
    CLASS-METHODS step1
      IMPORTING
        !iv_from         TYPE wdy_config_id
        !iv_to           TYPE wdy_config_id
      EXPORTING
        !es_fpm_name_map TYPE ts_fpm_name_map
        !es_replace_rule TYPE ts_replace_rule .
    CLASS-METHODS step2
      IMPORTING
        !iv_from                   TYPE wdy_config_id
        !is_replace_rule           TYPE ts_replace_rule
        !iv_feeder_class_copy_mode TYPE char1 DEFAULT 'C'
      EXPORTING
        !et_uibb_name_map          TYPE tt_uibb_name_map .
    CLASS-METHODS step3
      IMPORTING
        !is_fpm_name_map  TYPE ts_fpm_name_map
        !it_uibb_name_map TYPE tt_uibb_name_map
        !iv_devclass      TYPE devclass OPTIONAL
        !iv_trkorr        TYPE trkorr OPTIONAL .
    CLASS-METHODS copy_wdya
      IMPORTING
        !iv_from     TYPE wdy_application_name
        !iv_to       TYPE wdy_application_name
        !iv_devclass TYPE devclass
        !iv_trkorr   TYPE trkorr .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA mo_config_hrchy TYPE REF TO cl_fpm_cfg_hrchy_brwsr_assist .
    CLASS-DATA mt_config_key TYPE wdy_config_keys .

    CLASS-METHODS replace
      IMPORTING
        !is_replace_rule TYPE ts_replace_rule
      CHANGING
        !cv_value        TYPE clike .
    CLASS-METHODS on_config_tool_finished
      FOR EVENT action_finished OF if_wd_config_tool_base
      IMPORTING
        !caller .
    CLASS-METHODS dequeue_all .
ENDCLASS.



CLASS ZCL_FPM_TOOLS_COPY IMPLEMENTATION.


  METHOD copy_wdya.
    DATA: lo_application     TYPE REF TO if_wdy_md_application,
          ls_wdy_application TYPE wdy_application,
          lo_prop_iter       TYPE REF TO if_object_collection_iterator,
          lo_prop            TYPE REF TO if_wdy_md_application_property,
          lt_param           TYPE cl_wdy_wb_application_util=>ttyp_application_parameter,
          ls_param           TYPE cl_wdy_wb_application_util=>stru_application_parameter.

    lo_application = cl_wdy_md_application=>get_object_by_key( iv_from ).
    lo_application->if_wdy_md_object~get_definition(
      IMPORTING
        definition = ls_wdy_application
    ).

    lo_prop_iter = lo_application->get_properties( )->get_values_iterator( ).
    WHILE lo_prop_iter->has_next( ) = abap_true.
      lo_prop ?= lo_prop_iter->get_next( ).
      ls_param-parameter_name = lo_prop->get_name( ).
      ls_param-parameter_value = lo_prop->get_value( ).
      INSERT ls_param INTO TABLE lt_param.
    ENDWHILE.


    cl_wdy_wb_application_util=>create_application(
      EXPORTING
        name                 = iv_to                 " Web Dynpro: Application Name
        description          = lo_application->if_wdy_md_object~get_description( )          " Web Dynpro: Short Description of an Object
        component_name       = ls_wdy_application-component       " Web Dynpro: Component Name
        interface_view_name  = ls_wdy_application-startup_view  " Web Dynpro : Name of Interface View
        startup_plug_name    = ls_wdy_application-startup_plug    " Web Dynpro : Name of Startup Plug
        devclass             = iv_devclass             " Package
        corrnr               = iv_trkorr               " Request/Task
        parameters           = lt_param           " Application Parameter
    ).

    DATA: p_object_type TYPE  wbobjtype.
    p_object_type-objtype_tr = 'WDYA'.
    p_object_type-subtype_wb = 'Y20'. " FPM appl.
    CALL FUNCTION 'WB_TREE_UPDATE_OBJECTLIST'
      EXPORTING
*       p_object_type       = swbm_c_type_wdy_application
        p_global_type       = p_object_type
        p_object_name       = iv_to
        p_operation         = swbm_c_op_insert
        p_package_name      = iv_devclass
        p_author            = sy-uname
      EXCEPTIONS
        error_occured       = 1
        invalid_operation   = 2
        no_objectlist_found = 3
        long_object_name    = 4
        OTHERS              = 5.
  ENDMETHOD.


  METHOD dequeue_all.
    DATA: ls_config_key TYPE wdy_config_key.

    LOOP AT mt_config_key INTO ls_config_key.
      CALL FUNCTION 'DEQUEUE_E_WDY_CONFCOMP'
        EXPORTING
          mode_wdy_config_data = 'X'
          config_id            = ls_config_key-config_id
          config_type          = ls_config_key-config_type
          config_var           = ls_config_key-config_var
          x_config_id          = abap_true
          x_config_type        = abap_true
          x_config_var         = abap_true.
    ENDLOOP.
    CLEAR: mt_config_key.

  ENDMETHOD.


  METHOD execute.
    DATA: ls_fpm_name_map  TYPE ts_fpm_name_map,
          ls_replace_rule  TYPE ts_replace_rule,
          lt_uibb_name_map TYPE tt_uibb_name_map.

    step1(
      EXPORTING
        iv_from         = iv_from
        iv_to           = iv_to
      IMPORTING
        es_fpm_name_map = ls_fpm_name_map
        es_replace_rule = ls_replace_rule
    ).
    CHECK: ls_fpm_name_map IS NOT INITIAL.

    step2(
      EXPORTING
        iv_from          = iv_from
        is_replace_rule  = ls_replace_rule
      IMPORTING
        et_uibb_name_map = lt_uibb_name_map
    ).

    step3(
      EXPORTING
        is_fpm_name_map  = ls_fpm_name_map
        it_uibb_name_map = lt_uibb_name_map
        iv_devclass      = iv_devclass
        iv_trkorr        = iv_trkorr
    ).
  ENDMETHOD.


  METHOD get_lcs.
    DATA: lcs         TYPE TABLE OF int4_table,
          lv_length_x TYPE i,
          lv_length_y TYPE i,
          lv_index_x  TYPE i,
          lv_index_y  TYPE i,
          lv_offset_x TYPE i,
          lv_offset_y TYPE i.
    FIELD-SYMBOLS: <lcs_line>  TYPE int4_table,
                   <lcs_value> TYPE i.

    CLEAR: ev_length, ev_x_offset, ev_y_offset.

    IF iv_x EQ iv_y.
      ev_length = strlen( iv_x ).
      RETURN.
    ENDIF.

*Longest Common Substring
*if i == 0 or j == 0:
*	LCS[i][j] = 0
*elif string_A[i] == string_B[j]:
*	LCS[i][j] = LCS[i - 1][j - 1] + 1
*else:
*	LCS[i][j] = 0

    lv_length_x = strlen( iv_x ) + 1.
    lv_length_y = strlen( iv_y ) + 1.

    DO lv_length_y TIMES.
      lv_index_y = sy-index.
      lv_offset_y = sy-index - 2.
      APPEND INITIAL LINE TO lcs ASSIGNING <lcs_line>.
      DO lv_length_x TIMES.
        lv_index_x = sy-index.
        lv_offset_x = sy-index - 2.
        APPEND 0 TO <lcs_line> ASSIGNING <lcs_value>.
        IF lv_index_x EQ 1 OR lv_index_y EQ 1.
        ELSEIF iv_x+lv_offset_x(1) EQ iv_y+lv_offset_y(1).
          <lcs_value> = lcs[ lv_index_y - 1 ][ lv_index_x - 1 ] + 1.
          IF <lcs_value> > ev_length.
            ev_length = <lcs_value>.
            ev_x_offset = lv_offset_x + 1 - ev_length.
            ev_y_offset = lv_offset_y + 1 - ev_length.
          ENDIF.
        ELSE.
          " 0
        ENDIF.
      ENDDO.
    ENDDO.

  ENDMETHOD.


  METHOD on_config_tool_finished.
    DATA: lv_lines      TYPE i,
          ls_config_key TYPE wdy_config_key.

    lv_lines = lines( mo_config_hrchy->mt_message ).
    ls_config_key-config_id = mo_config_hrchy->mt_message[ lv_lines ]-msgv2.

    CALL FUNCTION 'ENQUEUE_E_WDY_CONFCOMP'
      EXPORTING
        mode_wdy_config_data = 'X'
        config_id            = ls_config_key-config_id
        config_type          = ls_config_key-config_type
        config_var           = ls_config_key-config_var
        x_config_id          = abap_true
        x_config_type        = abap_true
        x_config_var         = abap_true
      EXCEPTIONS
        foreign_lock         = 1
        system_failure       = 2
        OTHERS               = 3.
    IF sy-subrc EQ 0.
      APPEND ls_config_key TO mt_config_key.
    ENDIF.
  ENDMETHOD.


  METHOD replace.
    DATA: lv_char100 TYPE char100,
          lv_offset  TYPE i.

    lv_char100 = cv_value.

    CASE lv_char100(1).
      WHEN '/'.
        FIND ALL OCCURRENCES OF '/' IN lv_char100 MATCH OFFSET lv_offset.
        lv_offset = lv_offset + 1.
        lv_char100 = lv_char100+lv_offset.
        CLEAR lv_offset.
      WHEN 'Z' OR 'Y'.
        lv_char100 = lv_char100+1.
    ENDCASE.
    IF is_replace_rule-ns_to IS NOT INITIAL.
      lv_char100 = is_replace_rule-ns_to && lv_char100.
      lv_offset = strlen( is_replace_rule-ns_to ).
    ENDIF.

    IF is_replace_rule-beg_from IS INITIAL.
      IF is_replace_rule-beg_to IS NOT INITIAL.
        lv_char100+lv_offset = is_replace_rule-beg_to && lv_char100+lv_offset.
      ENDIF.
    ELSE.
      REPLACE is_replace_rule-beg_from IN SECTION OFFSET lv_offset OF lv_char100 WITH is_replace_rule-beg_to.
      IF sy-subrc EQ 0.
        lv_offset = lv_offset + strlen( is_replace_rule-beg_to ).
      ENDIF.
    ENDIF.

    IF is_replace_rule-end_from IS INITIAL.
      IF is_replace_rule-end_to IS NOT INITIAL.
        lv_char100 = lv_char100 && is_replace_rule-end_to.
      ENDIF.
    ELSE.
      FIND ALL OCCURRENCES OF is_replace_rule-end_from IN SECTION OFFSET lv_offset OF lv_char100 MATCH OFFSET lv_offset.
      IF sy-subrc EQ 0.
        REPLACE is_replace_rule-end_from IN SECTION OFFSET lv_offset OF lv_char100 WITH is_replace_rule-end_to.
      ENDIF.
    ENDIF.

    cv_value = lv_char100.
  ENDMETHOD.


  METHOD step1.
    DATA: lv_to             TYPE wdy_config_id,
          lv_x              TYPE string,
          lv_y              TYPE string,
          lv_ns_from_length TYPE i,
          lv_length         TYPE i,
          lv_x_offset       TYPE i,
          lv_y_offset       TYPE i.

    CLEAR: es_fpm_name_map, es_replace_rule.


    SELECT SINGLE application
      INTO es_fpm_name_map-application
      FROM wdy_config_appl
      WHERE config_id = iv_from
        AND config_type = '02'
        AND config_var = ''
    .
    IF sy-subrc <> 0.
      " not found
      RETURN.
    ENDIF.


    IF iv_to IS NOT INITIAL.
      lv_to = iv_to.
    ELSE.
      lv_to = iv_from.
      CASE lv_to(1).
        WHEN '/'.
          REPLACE REGEX '/.*/' IN lv_to WITH 'Z'.
        WHEN 'Z' OR 'Y'.
        WHEN OTHERS.
          lv_to = 'Z' && lv_to.
      ENDCASE.
    ENDIF.

    lv_x = iv_from.
    lv_y = lv_to.

    " namespace part
    CASE iv_from(1).
      WHEN '/'.
        FIND ALL OCCURRENCES OF '/' IN lv_x MATCH OFFSET lv_x_offset.
        lv_x_offset = lv_x_offset + 1.
*        es_replace_rule-ns_from = lv_x(lv_x_offset).
        lv_ns_from_length = lv_x_offset.
        lv_x = lv_x+lv_x_offset.
      WHEN 'Z' OR 'Y'.
        lv_x_offset = 1.
*        es_replace_rule-ns_from = lv_x(lv_x_offset).
        lv_ns_from_length = lv_x_offset.
        lv_x = lv_x+lv_x_offset.
    ENDCASE.
    CASE lv_to(1).
      WHEN '/'.
        FIND ALL OCCURRENCES OF '/' IN lv_y MATCH OFFSET lv_y_offset.
        lv_y_offset = lv_y_offset + 1.
        es_replace_rule-ns_to = lv_y(lv_y_offset).
        lv_y = lv_y+lv_y_offset.
      WHEN 'Z' OR 'Y'.
        lv_y_offset = 1.
        es_replace_rule-ns_to = lv_y(lv_y_offset).
        lv_y = lv_y+lv_y_offset.
    ENDCASE.


    IF lv_x <> lv_y.
      " remove tail number
      REPLACE REGEX '\d+$' IN lv_x WITH ''.
      REPLACE REGEX '\d+$' IN lv_y WITH ''.

      " 최장 공통 문자열(Longest Common Substring)
      get_lcs(
        EXPORTING
          iv_x        = lv_x
          iv_y        = lv_y
        IMPORTING
          ev_length   = lv_length
          ev_x_offset = lv_x_offset
          ev_y_offset = lv_y_offset
      ).

      " begin part
      IF lv_x_offset > 0.
        es_replace_rule-beg_from = lv_x(lv_x_offset).
      ENDIF.
      IF lv_y_offset > 0.
        es_replace_rule-beg_to = lv_y(lv_y_offset).
      ENDIF.
      IF es_replace_rule-end_from EQ es_replace_rule-beg_to.
        CLEAR: es_replace_rule-end_from, es_replace_rule-beg_to.
      ENDIF.

      " end part
*      lv_x_offset = strlen( es_replace_rule-ns_from ) + lv_x_offset + lv_length.
      lv_x_offset = lv_ns_from_length + lv_x_offset + lv_length.
      IF lv_x_offset < 32.
        es_replace_rule-end_from = iv_from+lv_x_offset.
      ENDIF.
      lv_y_offset = strlen( es_replace_rule-ns_to ) + lv_y_offset + lv_length.
      IF lv_y_offset < 32.
        es_replace_rule-end_to = lv_to+lv_y_offset.
      ENDIF.
      IF es_replace_rule-end_from EQ es_replace_rule-end_to.
        CLEAR: es_replace_rule-end_from, es_replace_rule-end_to.
      ENDIF.
    ENDIF.



    es_fpm_name_map-config_id = iv_from.
    zcl_fpm_tools=>suggest_object_name(
      EXPORTING
        iv_name        = lv_to
        iv_typekind    = 'WDCA'
      RECEIVING
        rv_name        = es_fpm_name_map-config_id_copy
    ).
    zcl_fpm_tools=>get_description(
      EXPORTING
        iv_name        = es_fpm_name_map-config_id
        iv_typekind    = 'WDCA'
      RECEIVING
        rv_description = es_fpm_name_map-config_description_copy
    ).
    es_fpm_name_map-application_copy = es_fpm_name_map-application.
    replace(
      EXPORTING
        is_replace_rule = es_replace_rule
      CHANGING
        cv_value        = es_fpm_name_map-application_copy
    ).
    zcl_fpm_tools=>suggest_object_name(
      EXPORTING
        iv_name        = es_fpm_name_map-application_copy
        iv_typekind    = 'WDYA'
      RECEIVING
        rv_name        = es_fpm_name_map-application_copy
    ).
    zcl_fpm_tools=>get_description(
      EXPORTING
        iv_name        = es_fpm_name_map-application
        iv_typekind    = 'WDYA'
      RECEIVING
        rv_description = es_fpm_name_map-application_description_copy
    ).

  ENDMETHOD.


  METHOD step2.
    DATA: lt_uibb_info      TYPE zcl_fpm_tools=>tt_uibb_info,
          ls_uibb_info      TYPE zcl_fpm_tools=>ts_uibb_info,
          ls_uibb_name_map  TYPE ts_uibb_name_map,
          ls_uibb_name_map2 TYPE ts_uibb_name_map.

    CLEAR: et_uibb_name_map.

**********************************************************************
* ET_UIBB_NAME_MAP
**********************************************************************
    zcl_fpm_tools=>get_fpm_tree(
      EXPORTING
        iv_wdca              = iv_from
        iv_read_feeder_class = abap_true
        iv_read_description  = abap_true
      IMPORTING
        et_uibb_info         = lt_uibb_info
    ).
    LOOP AT lt_uibb_info INTO ls_uibb_info.
      CLEAR: ls_uibb_name_map.
      ls_uibb_name_map-config_id = ls_uibb_info-config_id.
      ls_uibb_name_map-feeder_class = ls_uibb_info-feeder_class.

      ls_uibb_name_map-config_id_copy = ls_uibb_info-config_id.
      ls_uibb_name_map-config_description_copy = ls_uibb_info-config_description.
      replace(
        EXPORTING
          is_replace_rule = is_replace_rule
        CHANGING
          cv_value        = ls_uibb_name_map-config_id_copy
      ).
      zcl_fpm_tools=>suggest_object_name(
        EXPORTING
          iv_name        = ls_uibb_name_map-config_id_copy
          iv_typekind    = 'WDCC'
        RECEIVING
          rv_name        = ls_uibb_name_map-config_id_copy
      ).
      WHILE line_exists( et_uibb_name_map[ config_id_copy = ls_uibb_name_map-config_id_copy ] ).
        zcl_fpm_tools=>suggest_object_name(
          EXPORTING
            iv_name        = ls_uibb_name_map-config_id_copy
            iv_typekind    = 'WDCC'
            iv_next_number = abap_true
          RECEIVING
            rv_name        = ls_uibb_name_map-config_id_copy
        ).
      ENDWHILE.


      IF ls_uibb_name_map-feeder_class IS NOT INITIAL AND iv_feeder_class_copy_mode IS NOT INITIAL.
        ls_uibb_name_map-feeder_class_copy = ls_uibb_info-feeder_class.
        ls_uibb_name_map-feeder_class_description_copy = ls_uibb_info-feeder_class_description.
        ls_uibb_name_map-feeder_class_copy_mode = iv_feeder_class_copy_mode.
        READ TABLE et_uibb_name_map INTO ls_uibb_name_map2 WITH KEY feeder_class = ls_uibb_name_map-feeder_class.
        IF sy-subrc EQ 0.
          " same feeder exists.
          ls_uibb_name_map-feeder_class_copy = ls_uibb_name_map2-feeder_class_copy.
          ls_uibb_name_map-feeder_class_description_copy = ls_uibb_name_map2-feeder_class_description_copy.
        ELSE.
          replace(
            EXPORTING
              is_replace_rule = is_replace_rule
            CHANGING
              cv_value        = ls_uibb_name_map-feeder_class_copy
          ).
          zcl_fpm_tools=>suggest_object_name(
            EXPORTING
              iv_name        = ls_uibb_name_map-feeder_class_copy
              iv_typekind    = 'CLAS'
            RECEIVING
              rv_name        = ls_uibb_name_map-feeder_class_copy
          ).
          WHILE line_exists( et_uibb_name_map[ feeder_class_copy = ls_uibb_name_map-feeder_class_copy ] ).
            zcl_fpm_tools=>suggest_object_name(
              EXPORTING
                iv_name        = ls_uibb_name_map-feeder_class_copy
                iv_typekind    = 'CLAS'
                iv_next_number = abap_true
              RECEIVING
                rv_name        = ls_uibb_name_map-feeder_class_copy
            ).
          ENDWHILE.
        ENDIF.
      ENDIF.

      APPEND ls_uibb_name_map TO et_uibb_name_map.
    ENDLOOP.


  ENDMETHOD.


  METHOD step3.
    DATA: lo_hrchy_brwsr          TYPE REF TO cl_fpm_cfg_hrchy_brwsr_assist,
          lt_uibb_name_map        TYPE tt_uibb_name_map,
          lt_uibb_name_map_feeder TYPE tt_uibb_name_map,
          ls_uibb_name_map        TYPE ts_uibb_name_map,
          lv_devclass	            TYPE devclass,
          lv_trkorr	              TYPE trkorr,
          ls_wdy_config_key       TYPE wdy_config_key,
          ls_class_copy_key       TYPE seoclskey,
          ls_class_key            TYPE seoclskey,
          ls_class                TYPE vseoclass,
          ls_inheritance          TYPE vseoextend,
          lt_objects              TYPE TABLE OF dwinactiv,
          ls_objects              TYPE dwinactiv,
          lv_index                TYPE i.
    FIELD-SYMBOLS: <ls_node_table> TYPE fpm_s_cfg_appl_hier_tree.

    LOOP AT it_uibb_name_map INTO ls_uibb_name_map.
      IF ls_uibb_name_map-config_id EQ ls_uibb_name_map-config_id_copy.
        CLEAR: ls_uibb_name_map-config_id_copy, ls_uibb_name_map-feeder_class_copy, ls_uibb_name_map-feeder_class_copy_mode.
      ENDIF.
      CHECK: ls_uibb_name_map-config_id_copy IS NOT INITIAL.

      IF ls_uibb_name_map-feeder_class IS INITIAL OR
         ls_uibb_name_map-feeder_class EQ ls_uibb_name_map-feeder_class_copy.
        CLEAR: ls_uibb_name_map-feeder_class_copy, ls_uibb_name_map-feeder_class_copy_mode.
      ENDIF.

      APPEND ls_uibb_name_map TO lt_uibb_name_map.
    ENDLOOP.
    SORT lt_uibb_name_map BY config_id.

**********************************************************************
* Package & Correction number
**********************************************************************
    ls_wdy_config_key-config_id = is_fpm_name_map-config_id_copy.
    ls_wdy_config_key-config_type = '02'.
    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        object              = ls_wdy_config_key         " Object name
        object_class        = 'WDCA'   " Object class (ABAP,SCUA,SCRP,DICT,FUNC.)
        mode                = 'I'          " I(nsert), if object new
        global_lock         = 'X'          " SPACE: small block (LIMU); 'x': g. bl. (R3TR)
        devclass            = iv_devclass          " Package
        korrnum             = iv_trkorr          " Correction number
      IMPORTING
        devclass            = lv_devclass       " Package
        korrnum             = lv_trkorr        " Correction number
      EXCEPTIONS
        cancelled           = 1              " Processing cancelled
        permission_failure  = 2              " No correction entry possible
        unknown_objectclass = 3              " Object class not recognised
        OTHERS              = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.


**********************************************************************
* Copy Class
**********************************************************************
    lt_uibb_name_map_feeder = lt_uibb_name_map.
    DELETE lt_uibb_name_map_feeder WHERE feeder_class_copy_mode IS INITIAL.
    SORT lt_uibb_name_map_feeder BY feeder_class_copy.
    DELETE ADJACENT DUPLICATES FROM lt_uibb_name_map_feeder COMPARING feeder_class_copy.

    LOOP AT lt_uibb_name_map_feeder INTO ls_uibb_name_map.

      IF ls_uibb_name_map-feeder_class_copy_mode EQ 'I'.
        IF zcl_fpm_tools=>can_inherit( ls_uibb_name_map-feeder_class ) EQ abap_false.
          ls_uibb_name_map-feeder_class_copy_mode = 'C'.
        ELSE.
          CLEAR: ls_class, ls_inheritance.
          ls_class-clsname = ls_uibb_name_map-feeder_class_copy.
          ls_class-exposure = 2.
          ls_class-fixpt = abap_true.
          ls_class-unicode = abap_true.
          ls_inheritance-clsname = ls_class-clsname.
          ls_inheritance-refclsname = ls_uibb_name_map-feeder_class.
          ls_inheritance-state = 1.
          CALL FUNCTION 'SEO_CLASS_CREATE_COMPLETE'
            EXPORTING
              corrnr          = lv_trkorr
              devclass        = lv_devclass
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
        ENDIF.
      ENDIF.

      IF ls_uibb_name_map-feeder_class_copy_mode EQ 'C'.
        ls_class_key-clsname = ls_uibb_name_map-feeder_class.
        ls_class_copy_key-clsname = ls_uibb_name_map-feeder_class_copy.
        CALL FUNCTION 'SEO_CLASS_COPY'
          EXPORTING
            clskey       = ls_class_key
            new_clskey   = ls_class_copy_key
          CHANGING
            corrnr       = lv_trkorr
            devclass     = lv_devclass
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
        ENDIF.
      ENDIF.

      zcl_fpm_tools=>set_description(
         EXPORTING
           iv_name        = ls_uibb_name_map-feeder_class_copy
           iv_typekind    = 'CLAS'
           iv_description = ls_uibb_name_map-feeder_class_description_copy
       ).
      CLEAR: ls_objects.
      ls_objects-object = 'CLAS'.
      ls_objects-obj_name = ls_uibb_name_map-feeder_class_copy.
      APPEND ls_objects TO lt_objects.

    ENDLOOP.

    " activate class
    IF lt_objects IS NOT INITIAL.
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
    ENDIF.



**********************************************************************
* Deep Copy UIBB
**********************************************************************
    IF 1 EQ 2.
      " copied from:
      cl_fpm_cfg_hrchy_brwsr_util=>deep_copy( VALUE #( ) ).
    ENDIF.

*   get instance
    CREATE OBJECT lo_hrchy_brwsr.

*   init data
    lo_hrchy_brwsr->mv_mode = 2.
    lo_hrchy_brwsr->init_affixes( ).
    lo_hrchy_brwsr->ms_config_key-config_id = is_fpm_name_map-config_id.
    lo_hrchy_brwsr->ms_config_key-config_type = '02'.
    lo_hrchy_brwsr->mv_trkorr = lv_trkorr.
    lo_hrchy_brwsr->mv_devclass = lv_devclass.

*   load source and copy it
    lo_hrchy_brwsr->load_configuration( lo_hrchy_brwsr->mc_level-conf ).
    LOOP AT lo_hrchy_brwsr->mt_node_table ASSIGNING <ls_node_table> WHERE copy = abap_true.
      lv_index = sy-tabix.
      IF <ls_node_table>-config_type EQ '02'.
        <ls_node_table>-target_config_id = is_fpm_name_map-config_id_copy.
      ELSE.
        READ TABLE lt_uibb_name_map INTO ls_uibb_name_map WITH KEY config_id = <ls_node_table>-config_id BINARY SEARCH.
        IF sy-subrc EQ 0 AND ls_uibb_name_map-config_id_copy IS NOT INITIAL.
          <ls_node_table>-target_config_id = ls_uibb_name_map-config_id_copy.
        ELSE.
          CLEAR: <ls_node_table>-copy, <ls_node_table>-target_config_id.
        ENDIF.
      ENDIF.
    ENDLOOP.

    SET HANDLER on_config_tool_finished FOR ALL INSTANCES.
    mo_config_hrchy = lo_hrchy_brwsr.
    lo_hrchy_brwsr->deep_copy( ).
    CLEAR: mo_config_hrchy.
    SET HANDLER on_config_tool_finished FOR ALL INSTANCES ACTIVATION abap_false.
    dequeue_all( ).

**   return target
*    read table lo_hrchy_brwsr->mt_node_conf with key is_top_node = abap_true into ls_item.
*    assert sy-subrc eq 0.
*    rs_target_config_key-config_id = ls_item-target_config_id.
*    rs_target_config_key-config_type = ls_item-config_type.

**********************************************************************
* set description
**********************************************************************
    zcl_fpm_tools=>set_description(
       EXPORTING
         iv_name        = is_fpm_name_map-config_id_copy
         iv_typekind    = 'WDCA'
         iv_description = is_fpm_name_map-config_description_copy
     ).
    LOOP AT lt_uibb_name_map INTO ls_uibb_name_map WHERE config_id_copy IS NOT INITIAL.
      zcl_fpm_tools=>set_description(
         EXPORTING
           iv_name        = ls_uibb_name_map-config_id_copy
           iv_typekind    = 'WDCC'
           iv_description = ls_uibb_name_map-config_description_copy
       ).
    ENDLOOP.

**********************************************************************
* Copy Application
**********************************************************************
    IF is_fpm_name_map-application_copy IS NOT INITIAL AND
       is_fpm_name_map-application_copy <> is_fpm_name_map-application.
      copy_wdya(
        EXPORTING
          iv_from     = is_fpm_name_map-application
          iv_to       = is_fpm_name_map-application_copy
          iv_devclass = lv_devclass
          iv_trkorr   = lv_trkorr
      ).
      zcl_fpm_tools=>set_application(
        EXPORTING
          iv_wdca = is_fpm_name_map-config_id_copy
          iv_appl = is_fpm_name_map-application_copy
      ).
      zcl_fpm_tools=>set_description(
         EXPORTING
           iv_name        = is_fpm_name_map-application_copy
           iv_typekind    = 'WDYA'
           iv_description = is_fpm_name_map-application_description_copy
       ).
    ENDIF.


**********************************************************************
* set feeder
**********************************************************************
    LOOP AT lt_uibb_name_map INTO ls_uibb_name_map WHERE feeder_class_copy_mode IS NOT INITIAL.
      zcl_fpm_tools=>set_feeder_class(
        EXPORTING
          iv_config_id        = ls_uibb_name_map-config_id_copy
          iv_feeder_class     = ls_uibb_name_map-feeder_class_copy
*          iv_feeder_param     = iv_feeder_param
          iv_set_feeder_param = abap_false
      ).
    ENDLOOP.


  ENDMETHOD.
ENDCLASS.
