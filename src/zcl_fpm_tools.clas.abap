class ZCL_FPM_TOOLS definition
  public
  create public .

public section.

  types:
    BEGIN OF ts_uibb_info,
        config_id                TYPE wdy_config_id,
        feeder_class             TYPE fpmgb_feeder_class,
        feeder_param             TYPE string,
        config_description       TYPE wdy_md_description,
        feeder_class_description TYPE wdy_md_description,
      END OF ts_uibb_info .
  types:
    tt_uibb_info TYPE TABLE OF ts_uibb_info .

  class-methods GET_FPM_TREE
    importing
      !IV_WDCA type WDY_CONFIG_ID
      !IV_READ_FEEDER_CLASS type FLAG optional
      !IV_READ_FEEDER_PARAM type FLAG optional
      !IV_READ_DESCRIPTION type FLAG optional
    exporting
      !ET_UIBB_INFO type TT_UIBB_INFO .
  class-methods GET_FEEDER_CLASS
    importing
      !IV_CONFIG_ID type WDY_CONFIG_ID
      !IV_READ_FEEDER_PARAM type FLAG default ABAP_TRUE
    exporting
      !EV_FEEDER_CLASS type SEOCLSNAME
      !EV_FEEDER_PARAM type STRING .
  class-methods SET_FEEDER_CLASS
    importing
      !IV_CONFIG_ID type WDY_CONFIG_ID
      !IV_FEEDER_CLASS type SEOCLSNAME
      !IV_FEEDER_PARAM type STRING optional
      !IV_SET_FEEDER_PARAM type FLAG default ABAP_TRUE .
  class-methods SUGGEST_OBJECT_NAME
    importing
      !IV_NAME type CLIKE
      !IV_TYPEKIND type DDTYPEKIND optional
      !IV_NEXT_NUMBER type FLAG optional
    returning
      value(RV_NAME) type STRING .
  class-methods SET_APPLICATION
    importing
      !IV_WDCA type WDY_CONFIG_ID
      !IV_APPL type WDY_APPLICATION_NAME .
  class-methods GET_DESCRIPTION
    importing
      !IV_NAME type CLIKE
      !IV_TYPEKIND type DDTYPEKIND
    returning
      value(RV_DESCRIPTION) type STRING .
  class-methods SET_DESCRIPTION
    importing
      !IV_NAME type CLIKE
      !IV_TYPEKIND type DDTYPEKIND
      !IV_DESCRIPTION type CLIKE .
  class-methods CORR_INSERT
    importing
      !IV_NAME type CLIKE
      !IV_TYPEKIND type DDTYPEKIND
    exporting
      !EV_DEVCLASS type DEVCLASS
      !EV_TRKORR type TRKORR .
  class-methods CAN_INHERIT
    importing
      !IV_CLASS type SEOCLSNAME
      !IV_MESSAGE type FLAG optional
    returning
      value(RV_CAN_INHERIT) type FLAG .
  class-methods DELETE_FPM_TREE
    importing
      !IV_WDCA type WDY_CONFIG_ID
      !IV_WITH_FEEDER_CLASS type FLAG optional
      !IV_NAME_SPACE type STRING optional .
  class-methods EXPORT_FPM_TREE
    importing
      !IV_WDCA type WDY_CONFIG_ID
      !IV_NAME_SPACE type STRING optional
    returning
      value(RV_ZIP) type XSTRING .
  class-methods IMPORT_FPM_TREE
    importing
      !IV_ZIP type XSTRING
      !IV_OVERWRITE type FLAG optional .
  class-methods SAVE_WDCC
    importing
      !IS_WDCC type WDY_CONFIG_DATA .
  class-methods SAVE_WDCA
    importing
      !IS_WDCA type WDY_CONFIG_APPL .
  PROTECTED SECTION.

    CLASS-METHODS add_json_to_zip
      IMPORTING
        !it_data  TYPE data
        !iv_table TYPE clike
        !io_zip   TYPE REF TO cl_abap_zip .
    CLASS-METHODS get_wdcc_xml_filename
      IMPORTING
        !iv_wdcc           TYPE wdy_config_id
      RETURNING
        VALUE(rv_filename) TYPE string .
    CLASS-METHODS get_wdca_xml_filename
      IMPORTING
        !iv_wdca           TYPE wdy_config_id
      RETURNING
        VALUE(rv_filename) TYPE string .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_FPM_TOOLS IMPLEMENTATION.


  METHOD add_json_to_zip.
    DATA: lv_json     TYPE string,
          lv_xstring  TYPE xstring,
          lv_filename TYPE string.

    lv_filename = 'TABL.' && iv_table && '.json'.
    /ui2/cl_json=>serialize(
      EXPORTING
        data             = it_data
        compress         = abap_true
      RECEIVING
        r_json           = lv_json
    ).
    cl_abap_conv_out_ce=>create( encoding = 'UTF-8' )->convert(
      EXPORTING
        data   = lv_json
      IMPORTING
        buffer = lv_xstring
    ).
    io_zip->add(
      EXPORTING
        name           = lv_filename
        content        = lv_xstring
    ).
  ENDMETHOD.


  METHOD corr_insert.
    DATA: ls_wdy_config_key TYPE wdy_config_key.
    FIELD-SYMBOLS: <lv_object> TYPE data.

    CHECK: iv_name IS NOT INITIAL AND iv_typekind IS NOT INITIAL.

    CASE iv_typekind.
      WHEN 'WDCA'.    " R3TR  WDCA  Web Dynpro Application Configuration
        IF cl_abap_datadescr=>get_data_type_kind( iv_name ) EQ cl_abap_datadescr=>typekind_struct1.
          MOVE-CORRESPONDING iv_name TO ls_wdy_config_key.
        ELSE.
          ls_wdy_config_key-config_id = iv_name.
          ls_wdy_config_key-config_type = '02'.
        ENDIF.
        ASSIGN ls_wdy_config_key TO <lv_object>.
      WHEN 'WDCC'.    " R3TR  WDCC  Web Dynpro Component Configuration
        IF cl_abap_datadescr=>get_data_type_kind( iv_name ) EQ cl_abap_datadescr=>typekind_struct1.
          MOVE-CORRESPONDING iv_name TO ls_wdy_config_key.
        ELSE.
          ls_wdy_config_key-config_id = iv_name.
          ls_wdy_config_key-config_type = '00'.
        ENDIF.
        ASSIGN ls_wdy_config_key TO <lv_object>.
      WHEN OTHERS.
        ASSIGN iv_name TO <lv_object>.
    ENDCASE.

    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        object              = <lv_object>         " Object name
        object_class        = iv_typekind
*       mode                = 'I'          " I(nsert), if object new
        global_lock         = 'X'          " SPACE: small block (LIMU); 'x': g. bl. (R3TR)
      IMPORTING
        devclass            = ev_devclass       " Package
        korrnum             = ev_trkorr        " Correction number
      EXCEPTIONS
        cancelled           = 1              " Processing cancelled
        permission_failure  = 2              " No correction entry possible
        unknown_objectclass = 3              " Object class not recognised
        OTHERS              = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


  METHOD delete_fpm_tree.
    DATA: lt_uibb_info      TYPE tt_uibb_info,
          ls_uibb_info      TYPE ts_uibb_info,
          lv_application    TYPE wdy_config_appl-application,
          ls_wdy_config_key TYPE wdy_config_key,
          ls_clskey         TYPE seoclskey,
          lv_corrnr         TYPE trkorr.

    SELECT SINGLE application
      INTO lv_application
      FROM wdy_config_appl
      WHERE config_id = iv_wdca
        AND config_type = '02'
        AND config_var = ''
    .
    IF sy-subrc <> 0.
      " not found
      RETURN.
    ENDIF.

    get_fpm_tree(
      EXPORTING
        iv_wdca              = iv_wdca
        iv_read_feeder_class = iv_with_feeder_class
      IMPORTING
        et_uibb_info         = lt_uibb_info
    ).

    LOOP AT lt_uibb_info INTO ls_uibb_info.
      " delete uibb config
      ls_wdy_config_key-config_id = ls_uibb_info-config_id.
      IF ls_wdy_config_key-config_id CP iv_name_space && '*'.
        cl_wdr_configuration_utils=>delete_config_4_comp(
          EXPORTING
            p_config_key       = ls_wdy_config_key
          EXCEPTIONS
            action_cancelled   = 1            " Activity canceled
            error_occurred     = 2            " Errors occurred
            object_not_found   = 3            " Object not found
            permission_failure = 4            " Authorization error
            object_locked      = 5            " Object Locked
            OTHERS             = 6
        ).
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
      ENDIF.

      " delete class
      IF ls_uibb_info-feeder_class IS NOT INITIAL.
        ls_clskey-clsname = ls_uibb_info-feeder_class.
        IF ls_clskey-clsname CP iv_name_space && '*'.
          CALL FUNCTION 'SEO_CLASS_DELETE_COMPLETE'
            EXPORTING
              clskey       = ls_clskey               " Class
            EXCEPTIONS
              not_existing = 1
              is_interface = 2
              db_error     = 3
              no_access    = 4
              other        = 5
              OTHERS       = 6.
          IF sy-subrc > 2.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

    " delete FPM config
    ls_wdy_config_key-config_id = iv_wdca.
    ls_wdy_config_key-config_type = '02'.
    IF ls_wdy_config_key-config_id CP iv_name_space && '*'.
      cl_wdr_configuration_utils=>delete_config_4_appl(
        EXPORTING
          p_config_key       = ls_wdy_config_key
        EXCEPTIONS
          action_cancelled   = 1            " Activity canceled
          error_occurred     = 2            " Errors occurred
          object_not_found   = 3            " Object not found
          permission_failure = 4            " Authorization error
          object_locked      = 5            " Object Locked
          OTHERS             = 6
      ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.

    " delete FPM appl.
    IF lv_application CP iv_name_space && '*'.
      SELECT SINGLE application
        INTO lv_application
        FROM wdy_config_appl
        WHERE application = lv_application.
      IF sy-subrc <> 0.
        " if it has no config.
        TRY.
            cl_wdy_wb_application_util=>delete_application(
              EXPORTING
                name   = lv_application
                corrnr = lv_corrnr
            ).
          CATCH cx_wdy_wb_appl_util_failure.
        ENDTRY.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD export_fpm_tree.
    DATA: lt_uibb_info        TYPE tt_uibb_info,
          ls_uibb_info        TYPE ts_uibb_info,
          lv_application      TYPE wdy_config_appl-application,
          ls_wdy_config_data  TYPE wdy_config_data,
          lt_wdy_config_data  TYPE TABLE OF wdy_config_data,
          lt_wdy_config_datt  TYPE TABLE OF wdy_config_datt,
          lt_wdy_config_compt TYPE TABLE OF wdy_config_compt,
          lt_wdy_conf_delc    TYPE TABLE OF wdy_conf_delc,
          ls_wdy_config_appl  TYPE wdy_config_appl,
          lt_wdy_config_appl  TYPE TABLE OF wdy_config_appl,
          lt_wdy_config_appt  TYPE TABLE OF wdy_config_appt,
          lt_wdy_conf_dela    TYPE TABLE OF wdy_conf_dela,
          lt_wdy_application  TYPE TABLE OF wdy_application,
          lt_wdy_applicationt TYPE TABLE OF wdy_applicationt,
          lt_wdy_app_property TYPE TABLE OF wdy_app_property,
          lv_xstring          TYPE xstring,
          lo_zip              TYPE REF TO cl_abap_zip.

    SELECT SINGLE application
      INTO lv_application
      FROM wdy_config_appl
      WHERE config_id = iv_wdca
        AND config_type = '02'
        AND config_var = ''
    .
    IF sy-subrc <> 0.
      " not found
      RETURN.
    ENDIF.

    CREATE OBJECT lo_zip.

    get_fpm_tree(
      EXPORTING
        iv_wdca              = iv_wdca
      IMPORTING
        et_uibb_info         = lt_uibb_info
    ).

    IF lt_uibb_info IS NOT INITIAL.
      " save uibb config
      LOOP AT lt_uibb_info INTO ls_uibb_info.
        CHECK: ls_uibb_info-config_id CP iv_name_space && '*'.
        SELECT SINGLE *
          INTO ls_wdy_config_data
          FROM wdy_config_data
          WHERE config_id = ls_uibb_info-config_id
            AND config_type = '00'
            AND config_var = ''
        .
        CHECK: sy-subrc EQ 0.
        lo_zip->add(
          EXPORTING
            name           = get_wdcc_xml_filename( ls_uibb_info-config_id )
            content        = ls_wdy_config_data-xcontent
        ).
        CLEAR: ls_wdy_config_data-xcontent.
        APPEND ls_wdy_config_data TO lt_wdy_config_data.

        SELECT *
          INTO TABLE lt_wdy_config_datt
          FROM wdy_config_datt
          WHERE config_id = ls_uibb_info-config_id
            AND config_type = '00'
            AND config_var = ''
        .
        SELECT *
          APPENDING TABLE lt_wdy_config_compt
          FROM wdy_config_compt
          WHERE config_id = ls_uibb_info-config_id
            AND config_type = '00'
            AND config_var = ''
        .
        SELECT *
          APPENDING TABLE lt_wdy_conf_delc
          FROM wdy_conf_delc
          WHERE config_id = ls_uibb_info-config_id
            AND config_type = '00'
            AND config_var = ''
        .
      ENDLOOP.

      IF lt_wdy_config_data IS NOT INITIAL.
        add_json_to_zip(
          EXPORTING
            it_data  = lt_wdy_config_data
            iv_table = 'WDY_CONFIG_DATA'
            io_zip   = lo_zip
        ).
        CLEAR: lt_wdy_config_data.
      ENDIF.

      IF lt_wdy_config_datt IS NOT INITIAL.
        add_json_to_zip(
          EXPORTING
            it_data  = lt_wdy_config_datt
            iv_table = 'WDY_CONFIG_DATT'
            io_zip   = lo_zip
        ).
        CLEAR: lt_wdy_config_datt.
      ENDIF.

      IF lt_wdy_config_compt IS NOT INITIAL.
        add_json_to_zip(
          EXPORTING
            it_data  = lt_wdy_config_compt
            iv_table = 'WDY_CONFIG_COMPT'
            io_zip   = lo_zip
        ).
        CLEAR: lt_wdy_config_compt.
      ENDIF.

      IF lt_wdy_conf_delc IS NOT INITIAL.
        add_json_to_zip(
          EXPORTING
            it_data  = lt_wdy_conf_delc
            iv_table = 'WDY_CONF_DELC'
            io_zip   = lo_zip
        ).
        CLEAR: lt_wdy_conf_delc.
      ENDIF.
    ENDIF.


    " save FPM config
    SELECT SINGLE *
      INTO ls_wdy_config_appl
      FROM wdy_config_appl
      WHERE config_id = iv_wdca
        AND config_type = '02'
        AND config_var = ''
    .
    lo_zip->add(
      EXPORTING
        name           = get_wdca_xml_filename( iv_wdca )
        content        = ls_wdy_config_appl-xcontent
    ).
    CLEAR: ls_wdy_config_appl-xcontent.
    APPEND ls_wdy_config_appl TO lt_wdy_config_appl.

    SELECT *
      INTO TABLE lt_wdy_config_appt
      FROM wdy_config_appt
      WHERE config_id = iv_wdca
        AND config_type = '02'
        AND config_var = ''
    .
    SELECT *
      APPENDING TABLE lt_wdy_conf_dela
      FROM wdy_conf_dela
      WHERE config_id = iv_wdca
        AND config_type = '02'
        AND config_var = ''
    .

    IF lt_wdy_config_appl IS NOT INITIAL.
      add_json_to_zip(
        EXPORTING
          it_data  = lt_wdy_config_appl
          iv_table = 'WDY_CONFIG_APPL'
          io_zip   = lo_zip
      ).
      CLEAR: lt_wdy_config_appl.
    ENDIF.

    IF lt_wdy_config_appt IS NOT INITIAL.
      add_json_to_zip(
        EXPORTING
          it_data  = lt_wdy_config_appt
          iv_table = 'WDY_CONFIG_APPT'
          io_zip   = lo_zip
      ).
      CLEAR: lt_wdy_config_appt.
    ENDIF.

    IF lt_wdy_conf_dela IS NOT INITIAL.
      add_json_to_zip(
        EXPORTING
          it_data  = lt_wdy_conf_dela
          iv_table = 'WDY_CONF_DELA'
          io_zip   = lo_zip
      ).
      CLEAR: lt_wdy_conf_dela.
    ENDIF.


    " save FPM appl.
    IF lv_application CP iv_name_space && '*'.
      SELECT *
        INTO TABLE lt_wdy_application
        FROM wdy_application
        WHERE application_name = lv_application.
      SELECT *
        INTO TABLE lt_wdy_applicationt
        FROM wdy_applicationt
        WHERE application_name = lv_application.
      SELECT *
        INTO TABLE lt_wdy_app_property
        FROM wdy_app_property
        WHERE application_name = lv_application.

      IF lt_wdy_application IS NOT INITIAL.
        add_json_to_zip(
          EXPORTING
            it_data  = lt_wdy_application
            iv_table = 'WDY_APPLICATION'
            io_zip   = lo_zip
        ).
        CLEAR: lt_wdy_application.
      ENDIF.

      IF lt_wdy_applicationt IS NOT INITIAL.
        add_json_to_zip(
          EXPORTING
            it_data  = lt_wdy_applicationt
            iv_table = 'WDY_APPLICATIONT'
            io_zip   = lo_zip
        ).
        CLEAR: lt_wdy_applicationt.
      ENDIF.

      IF lt_wdy_app_property IS NOT INITIAL.
        add_json_to_zip(
          EXPORTING
            it_data  = lt_wdy_app_property
            iv_table = 'WDY_APP_PROPERTY'
            io_zip   = lo_zip
        ).
        CLEAR: lt_wdy_app_property.
      ENDIF.
    ENDIF.


    rv_zip = lo_zip->save( ).
  ENDMETHOD.


  METHOD get_description.

    CASE iv_typekind.
      WHEN 'WDCA'.    " R3TR  WDCA  Web Dynpro Application Configuration
        SELECT SINGLE description
          INTO rv_description
          FROM wdy_config_appt
          WHERE config_id = iv_name
            AND config_type = '02'
            AND config_var = ''
            AND langu = sy-langu.
      WHEN 'WDCC'.    " R3TR  WDCC  Web Dynpro Component Configuration
        SELECT SINGLE description
          INTO rv_description
          FROM wdy_config_datt
          WHERE config_id = iv_name
            AND config_type = '00'
            AND config_var = ''
            AND langu = sy-langu.
      WHEN 'WDYA'.    " R3TR  WDYA  Web Dynpro Application
        SELECT SINGLE description
          INTO rv_description
          FROM wdy_applicationt
          WHERE application_name = iv_name
            AND langu = sy-langu.
      WHEN 'WDYN'.    " R3TR  WDYN  Web Dynpro Component
        SELECT SINGLE description
          INTO rv_description
          FROM wdy_componentt
          WHERE component_name = iv_name
            AND langu = sy-langu.
      WHEN 'CLAS'.    " R3TR  CLAS  Class (ABAP Objects)
        SELECT SINGLE descript
          INTO rv_description
          FROM seoclasstx
          WHERE clsname = iv_name
            AND langu = sy-langu.
      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.


  METHOD get_feeder_class.
    DATA: ls_wdcc    TYPE wdy_config_data,
          lv_string  TYPE string,
          lv_offset  TYPE i,
          lv_offset2 TYPE i,
          lv_length  TYPE i.

    CLEAR: ev_feeder_class, ev_feeder_param.

    SELECT SINGLE *
      INTO ls_wdcc
      FROM wdy_config_data
      WHERE config_id = iv_config_id
        AND config_type = '00'
        AND config_var = ''
    .
    CHECK: sy-subrc EQ 0.

    lv_string = cl_wdr_configuration_utils=>xml_xstring2string( in_xstring = ls_wdcc-xcontent ).

    CASE ls_wdcc-component.
      WHEN if_fpm_cfg_constants=>gc_component_name-composite
        OR if_fpm_cfg_constants=>gc_component_name-tabbed
        OR if_fpm_cfg_constants=>gc_component_name-oif
        OR if_fpm_cfg_constants=>gc_component_name-gaf
        OR if_fpm_cfg_constants=>gc_component_name-ovp.
        " find appcc
        FIND '<Node Name="APP_SPECIFIC_CC"' IN lv_string MATCH OFFSET lv_offset.
        IF sy-subrc EQ 0.
          FIND '>' IN SECTION OFFSET lv_offset OF lv_string MATCH OFFSET lv_offset2.
          lv_offset2 = lv_offset2 - 1.
          IF lv_string+lv_offset2(1) EQ '/'.
          ELSE.
            FIND '</Node>' IN SECTION OFFSET lv_offset2 OF lv_string MATCH OFFSET lv_offset2.
            lv_length = lv_offset2 - lv_offset.
            FIND REGEX '<COMPONENT>[^<]*</COMPONENT>' IN SECTION OFFSET lv_offset LENGTH lv_length OF lv_string MATCH OFFSET lv_offset MATCH LENGTH lv_length.
            IF sy-subrc EQ 0.
              lv_offset = lv_offset + 11.
              lv_length = lv_length - 23.
              ev_feeder_class = lv_string+lv_offset(lv_length).
              SELECT SINGLE component_name
                INTO ev_feeder_class
                FROM wdy_component
                WHERE component_name = ev_feeder_class.
              IF sy-subrc EQ 0.
                CLEAR: ev_feeder_class.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      WHEN OTHERS.
        " find feeder
        FIND REGEX '<FEEDER>[^<]*</FEEDER>' IN lv_string MATCH OFFSET lv_offset MATCH LENGTH lv_length.
        IF sy-subrc EQ 0.
          lv_offset = lv_offset + 8.
          lv_length = lv_length - 17.
          ev_feeder_class = lv_string+lv_offset(lv_length).
          " find param
          IF iv_read_feeder_param EQ abap_true.
            FIND '<Node Name="PARAMETER"' IN lv_string MATCH OFFSET lv_offset.
            IF sy-subrc EQ 0.
              FIND '>' IN SECTION OFFSET lv_offset OF lv_string MATCH OFFSET lv_offset2.
              lv_offset2 = lv_offset2 - 1.
              IF lv_string+lv_offset2(1) EQ '/'.
              ELSE.
                lv_offset = lv_offset2 + 2.
                FIND '</Node>' IN SECTION OFFSET lv_offset2 OF lv_string MATCH OFFSET lv_offset2.
                lv_length = lv_offset2 - lv_offset.
                ev_feeder_param = lv_string+lv_offset(lv_length).
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
    ENDCASE.
  ENDMETHOD.


  METHOD get_fpm_tree.
    DATA: lo_hrchy_brwsr TYPE REF TO cl_fpm_cfg_hrchy_brwsr_assist,
          lo_component   TYPE REF TO cl_wdr_component,
          ls_uibb_info   TYPE ts_uibb_info.
    FIELD-SYMBOLS: <ls_uibb_info>  TYPE ts_uibb_info,
                   <ls_node_table> TYPE fpm_s_cfg_appl_hier_tree.
    CLEAR: et_uibb_info.


    CREATE OBJECT lo_hrchy_brwsr.
    lo_hrchy_brwsr->mv_mode = 2.
    lo_hrchy_brwsr->init_affixes( ).
    lo_hrchy_brwsr->ms_config_key-config_id = iv_wdca.
    lo_hrchy_brwsr->ms_config_key-config_type = '02'.
    CREATE OBJECT lo_hrchy_brwsr->mo_message_manager TYPE cl_wdr_message_manager
      EXPORTING
        component = lo_component.
    TRY.
        lo_hrchy_brwsr->load_configuration( lo_hrchy_brwsr->mc_level-conf ).
      CATCH cx_wd_config_tool.
        RETURN.
      CATCH cx_wd_abort_message_manager.
        RETURN.
    ENDTRY.
    LOOP AT lo_hrchy_brwsr->mt_node_table ASSIGNING <ls_node_table> WHERE config_type = '00' AND is_configurable = abap_true.
      ls_uibb_info-config_id = <ls_node_table>-config_id.
      APPEND ls_uibb_info TO et_uibb_info.
    ENDLOOP.


    CHECK: et_uibb_info IS NOT INITIAL.
    SORT et_uibb_info BY config_id.
    DELETE ADJACENT DUPLICATES FROM et_uibb_info COMPARING config_id.
    " find top layout config
    LOOP AT lo_hrchy_brwsr->mt_node_table ASSIGNING <ls_node_table>
        WHERE component_name = if_fpm_cfg_constants=>gc_component_name-oif
           OR component_name = if_fpm_cfg_constants=>gc_component_name-gaf
           OR component_name = if_fpm_cfg_constants=>gc_component_name-ovp.
      EXIT.
    ENDLOOP.
    READ TABLE et_uibb_info INTO ls_uibb_info WITH KEY config_id = <ls_node_table>-config_id BINARY SEARCH.
    IF sy-subrc EQ 0.
      DELETE et_uibb_info INDEX sy-tabix.
      INSERT ls_uibb_info INTO et_uibb_info INDEX 1.
    ENDIF.


    IF iv_read_description EQ abap_true.
      LOOP AT et_uibb_info ASSIGNING <ls_uibb_info>.
        get_description(
           EXPORTING
             iv_name        = <ls_uibb_info>-config_id
             iv_typekind    = 'WDCC'
           RECEIVING
             rv_description = <ls_uibb_info>-config_description
         ).
      ENDLOOP.
    ENDIF.

    IF iv_read_feeder_class EQ abap_true.
      LOOP AT et_uibb_info ASSIGNING <ls_uibb_info>.
        get_feeder_class(
          EXPORTING
            iv_config_id    = <ls_uibb_info>-config_id
            iv_read_feeder_param = iv_read_feeder_param
          IMPORTING
            ev_feeder_class = <ls_uibb_info>-feeder_class
            ev_feeder_param = <ls_uibb_info>-feeder_param
        ).
        IF iv_read_description EQ abap_true.
          get_description(
             EXPORTING
               iv_name        = <ls_uibb_info>-feeder_class
               iv_typekind    = 'CLAS'
             RECEIVING
               rv_description = <ls_uibb_info>-feeder_class_description
           ).
        ENDIF.
      ENDLOOP.
    ENDIF.


  ENDMETHOD.


  METHOD get_wdca_xml_filename.
    rv_filename = iv_wdca.
    IF iv_wdca(1) EQ '/'.
      REPLACE ALL OCCURRENCES OF '/' IN rv_filename WITH '#'.
    ENDIF.
    rv_filename = 'WDCA.' && rv_filename && '.xml'.
  ENDMETHOD.


  METHOD get_wdcc_xml_filename.
    rv_filename = iv_wdcc.
    IF iv_wdcc(1) EQ '/'.
      REPLACE ALL OCCURRENCES OF '/' IN rv_filename WITH '#'.
    ENDIF.
    rv_filename = 'WDCC.' && rv_filename && '.xml'.
  ENDMETHOD.


  METHOD import_fpm_tree.
    DATA: lt_uibb_info        TYPE tt_uibb_info,
          ls_uibb_info        TYPE ts_uibb_info,
          lv_application      TYPE wdy_config_appl-application,
          ls_wdy_config_data  TYPE wdy_config_data,
          lt_wdy_config_data  TYPE TABLE OF wdy_config_data,
          lt_wdy_config_datt  TYPE TABLE OF wdy_config_datt,
          lt_wdy_config_compt TYPE TABLE OF wdy_config_compt,
          lt_wdy_conf_delc    TYPE TABLE OF wdy_conf_delc,
          ls_wdy_config_appl  TYPE wdy_config_appl,
          lt_wdy_config_appl  TYPE TABLE OF wdy_config_appl,
          lt_wdy_config_appt  TYPE TABLE OF wdy_config_appt,
          lt_wdy_conf_dela    TYPE TABLE OF wdy_conf_dela,
          lt_wdy_application  TYPE TABLE OF wdy_application,
          lt_wdy_applicationt TYPE TABLE OF wdy_applicationt,
          lt_wdy_app_property TYPE TABLE OF wdy_app_property,
          ls_wdy_config_key   TYPE wdy_config_key,
          lv_devclass	        TYPE devclass,
          lv_trkorr	          TYPE trkorr,
          lv_korrnum          TYPE tadir-korrnum,
          lt_tadir            TYPE TABLE OF tadir,
          ls_tadir            TYPE tadir,
          lv_json             TYPE string,
          lv_xstring          TYPE xstring,
          ls_file             TYPE cl_abap_zip=>t_file,
          lv_table            TYPE string,
          lv_index            TYPE i,
          lo_zip              TYPE REF TO cl_abap_zip.
    FIELD-SYMBOLS: <lt_data>            TYPE table,
                   <ls_wdy_config_data> TYPE wdy_config_data,
                   <ls_wdy_config_appl> TYPE wdy_config_appl,
                   <ls_wdy_application> TYPE wdy_application.

**********************************************************************
* read ZIP
**********************************************************************
    CREATE OBJECT lo_zip.
    lo_zip->load(
      EXPORTING
        zip             = iv_zip
      EXCEPTIONS
        zip_parse_error = 1
        OTHERS          = 2
    ).
    CHECK: sy-subrc EQ 0.

    LOOP AT lo_zip->files INTO ls_file WHERE name CP 'TABL.*'.
      lv_index = sy-tabix.
      lo_zip->get(
        EXPORTING
          index                   = lv_index
        IMPORTING
          content                 = lv_xstring
        EXCEPTIONS
          zip_index_error         = 1
          zip_decompression_error = 2
          OTHERS                  = 3
      ).
      CHECK: sy-subrc EQ 0.
      cl_abap_conv_in_ce=>create( encoding = 'UTF-8' input = lv_xstring )->read(
        IMPORTING
          data = lv_json
      ).

      lv_table = 'LT_' && ls_file-name+5.
      REPLACE '.json' IN lv_table WITH ''.
      ASSIGN (lv_table) TO <lt_data>.

      /ui2/cl_json=>deserialize(
        EXPORTING
          json             = lv_json
        CHANGING
          data             = <lt_data>
      ).
    ENDLOOP.

*cl_wdr_configuration_utils=>delete_config_4_appl

**********************************************************************
* overwrite
**********************************************************************
    LOOP AT lt_wdy_config_appl ASSIGNING <ls_wdy_config_appl>.
      SELECT SINGLE config_id
        INTO ls_wdy_config_key-config_id
        FROM wdy_config_appl
        WHERE config_id = <ls_wdy_config_appl>-config_id
          AND config_type = <ls_wdy_config_appl>-config_type
          AND config_var = <ls_wdy_config_appl>-config_var.
      IF sy-subrc EQ 0.
        IF iv_overwrite EQ abap_false.
          MESSAGE <ls_wdy_config_appl>-config_id && ` exists.` TYPE 'E'.
        ELSE.
          MOVE-CORRESPONDING <ls_wdy_config_appl> TO ls_wdy_config_key.
          cl_wdr_configuration_utils=>delete_config_4_appl(
            EXPORTING
              p_config_key       = ls_wdy_config_key
            EXCEPTIONS
              action_cancelled   = 1            " Activity canceled
              error_occurred     = 2            " Errors occurred
              object_not_found   = 3            " Object not found
              permission_failure = 4            " Authorization error
              object_locked      = 5            " Object Locked
              OTHERS             = 6
          ).
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
    LOOP AT lt_wdy_config_data ASSIGNING <ls_wdy_config_data>.
      SELECT SINGLE config_id
        INTO ls_wdy_config_key-config_id
        FROM wdy_config_data
        WHERE config_id = <ls_wdy_config_data>-config_id
          AND config_type = <ls_wdy_config_data>-config_type
          AND config_var = <ls_wdy_config_data>-config_var.
      IF sy-subrc EQ 0.
        IF iv_overwrite EQ abap_false.
          MESSAGE <ls_wdy_config_data>-config_id && ` exists.` TYPE 'E'.
        ELSE.
          MOVE-CORRESPONDING <ls_wdy_config_data> TO ls_wdy_config_key.
          cl_wdr_configuration_utils=>delete_config_4_comp(
            EXPORTING
              p_config_key       = ls_wdy_config_key
            EXCEPTIONS
              action_cancelled   = 1            " Activity canceled
              error_occurred     = 2            " Errors occurred
              object_not_found   = 3            " Object not found
              permission_failure = 4            " Authorization error
              object_locked      = 5            " Object Locked
              OTHERS             = 6
          ).
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
    LOOP AT lt_wdy_application ASSIGNING <ls_wdy_application>.
      SELECT SINGLE application_name
        INTO lv_application
        FROM wdy_application
        WHERE application_name = <ls_wdy_application>-application_name.
      IF sy-subrc EQ 0.
        " application exist. OK.
        DELETE lt_wdy_application.
      ENDIF.
    ENDLOOP.


**********************************************************************
* TADIR
**********************************************************************
    LOOP AT lt_wdy_config_appl ASSIGNING <ls_wdy_config_appl>.
      MOVE-CORRESPONDING <ls_wdy_config_appl> TO ls_wdy_config_key.
      ls_tadir-object = 'WDCA'.
      ls_tadir-obj_name = ls_wdy_config_key.
      APPEND ls_tadir TO lt_tadir.
    ENDLOOP.
    LOOP AT lt_wdy_config_data ASSIGNING <ls_wdy_config_data>.
      MOVE-CORRESPONDING <ls_wdy_config_data> TO ls_wdy_config_key.
      ls_tadir-object = 'WDCC'.
      ls_tadir-obj_name = ls_wdy_config_key.
      APPEND ls_tadir TO lt_tadir.
    ENDLOOP.
    LOOP AT lt_wdy_application ASSIGNING <ls_wdy_application>.
      ls_tadir-object = 'WDYA'.
      ls_tadir-obj_name = <ls_wdy_application>-application_name.
      APPEND ls_tadir TO lt_tadir.
    ENDLOOP.

    LOOP AT lt_tadir INTO ls_tadir.
      IF sy-tabix EQ 1.
        CALL FUNCTION 'RS_ACCESS_PERMISSION'
          EXPORTING
            authority_check          = 'X'                   " Check authorization ('X'->yes, ' '->no)  CHAR 1
            global_lock              = 'X'                   " Great corr. lock (R3TR); space:LIMU-Sp.  CHAR 1
            master_language          = sy-langu                 " Maintenance language of overall object
            mode                     = 'INSERT'              " Mode ('INSERT','MODIFY','SHOW','FREE')   CHAR 6
            object                   = ls_tadir-obj_name                " Object (complete lock key)               CHAR 40
            object_class             = ls_tadir-object          " Obj. clss (ABAP,SCUA,SCRP,SLDB,T100... ) CHAR 4
          EXCEPTIONS
            canceled_in_corr         = 1                     " User canceled correction system dialog box (F12
            enqueued_by_user         = 2                     " Object locked by other user
            enqueue_system_failure   = 3                     " Internal error in ENQUEUE function module
            illegal_parameter_values = 4                     " Invalid value for MODE or OBJECT_CLASS
            locked_by_author         = 5                     " Editor lock is set (only for SE38)
            no_modify_permission     = 6                     " User has no change authorization
            no_show_permission       = 7                     " User has no display authorization
            permission_failure       = 8                     " General exception: Required access denied
            request_language_denied  = 9                     " Unpermitted request language
            OTHERS                   = 10.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
        CALL FUNCTION 'RS_CORR_INSERT'
          EXPORTING
            object              = ls_tadir-obj_name         " Object name
            object_class        = ls_tadir-object   " Object class (ABAP,SCUA,SCRP,DICT,FUNC.)
            mode                = 'I'          " I(nsert), if object new
            global_lock         = 'X'          " SPACE: small block (LIMU); 'x': g. bl. (R3TR)
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
        ENDIF.
        lv_korrnum = lv_trkorr.
      ELSE.
        CALL FUNCTION 'TR_TADIR_INTERFACE'
          EXPORTING
            wi_test_modus                  = ' '               " X - simulation mode (no update)
            wi_tadir_pgmid                 = 'R3TR'    " Input for TADIR field PGMID
            wi_tadir_object                = ls_tadir-object   " Input for TADIR field OBJECT
            wi_tadir_obj_name              = ls_tadir-obj_name " Input for TADIR field OBJ_NAME
            wi_tadir_korrnum               = lv_korrnum               " Transport Organizer internal
            wi_tadir_devclass              = lv_devclass               " Input for TADIR field DEVCLASS
          EXCEPTIONS
            tadir_entry_not_existing       = 1                 " Object directory entry does not exist
            tadir_entry_ill_type           = 2                 " Transferred TADIR key not compatible with E071
            no_systemname                  = 3                 " System name not found
            no_systemtype                  = 4                 " System type not defined
            original_system_conflict       = 5                 " Object already exists in another system
            object_reserved_for_devclass   = 6                 " Object reserved for name range
            object_exists_global           = 7                 " Object exists globally
            object_exists_local            = 8                 " Object exists locally
            object_is_distributed          = 9                 " Object is distributed
            obj_specification_not_unique   = 10                " Object specification for import is not sufficient
            no_authorization_to_delete     = 11                " No permission to delete
            devclass_not_existing          = 12                " Package unknown
            simultanious_set_remove_repair = 13                " Repair flag set/reset simultaneously
            order_missing                  = 14                " Repair request was not transferred
            no_modification_of_head_syst   = 15                " Modification of HEAD-SYST entry not allowed
            pgmid_object_not_allowed       = 16                " PGMID entry not permitted
            masterlanguage_not_specified   = 17                " Master language not specified
            devclass_not_specified         = 18                " Package not specified
            specify_owner_unique           = 19
            loc_priv_objs_no_repair        = 20                " No repair to local-private objects
            gtadir_not_reached             = 21                " The GTADIR cannot be accessed
            object_locked_for_order        = 22
            change_of_class_not_allowed    = 23
            no_change_from_sap_to_tmp      = 24                " Do not switch SAP objects to customer development class
            OTHERS                         = 25.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
      ENDIF.
    ENDLOOP.



**********************************************************************
* save
**********************************************************************
    " save FPM config
    LOOP AT lt_wdy_config_appl ASSIGNING <ls_wdy_config_appl>.
      lo_zip->get(
        EXPORTING
          name                    = get_wdca_xml_filename( <ls_wdy_config_appl>-config_id )
        IMPORTING
          content                 = <ls_wdy_config_appl>-xcontent
        EXCEPTIONS
          zip_index_error         = 1
          zip_decompression_error = 2
          OTHERS                  = 3
      ).

      DELETE FROM wdy_config_appl WHERE config_id = <ls_wdy_config_appl>-config_id AND config_type = <ls_wdy_config_appl>-config_type AND config_var = <ls_wdy_config_appl>-config_var.
      DELETE FROM wdy_config_appt WHERE config_id = <ls_wdy_config_appl>-config_id AND config_type = <ls_wdy_config_appl>-config_type AND config_var = <ls_wdy_config_appl>-config_var.
      DELETE FROM wdy_conf_dela WHERE config_id = <ls_wdy_config_appl>-config_id AND config_type = <ls_wdy_config_appl>-config_type AND config_var = <ls_wdy_config_appl>-config_var.
    ENDLOOP.


    " save uibb config
    LOOP AT lt_wdy_config_data ASSIGNING <ls_wdy_config_data>.
      lo_zip->get(
        EXPORTING
          name                    = get_wdcc_xml_filename( <ls_wdy_config_data>-config_id )
        IMPORTING
          content                 = <ls_wdy_config_data>-xcontent
        EXCEPTIONS
          zip_index_error         = 1
          zip_decompression_error = 2
          OTHERS                  = 3
      ).

      DELETE FROM wdy_config_data WHERE config_id = <ls_wdy_config_data>-config_id AND config_type = <ls_wdy_config_data>-config_type AND config_var = <ls_wdy_config_data>-config_var.
      DELETE FROM wdy_config_datt WHERE config_id = <ls_wdy_config_data>-config_id AND config_type = <ls_wdy_config_data>-config_type AND config_var = <ls_wdy_config_data>-config_var.
      DELETE FROM wdy_config_compt WHERE config_id = <ls_wdy_config_data>-config_id AND config_type = <ls_wdy_config_data>-config_type AND config_var = <ls_wdy_config_data>-config_var.
      DELETE FROM wdy_conf_delc WHERE config_id = <ls_wdy_config_data>-config_id AND config_type = <ls_wdy_config_data>-config_type AND config_var = <ls_wdy_config_data>-config_var.
    ENDLOOP.


    " save FPM appl.
    LOOP AT lt_wdy_application ASSIGNING <ls_wdy_application>.

      IF cl_wdy_md_application=>check_existency( <ls_wdy_application>-application_name ) EQ abap_true.
        cl_wdy_md_application=>delete_sicf(
          EXPORTING
            p_applname                  = <ls_wdy_application>-application_name  " Name of WDY Application
            p_dark                      = abap_true
          EXCEPTIONS
            enqueue_error               = 1           " Error in Database Enqueue
            node_not_existing           = 0           " Node does not exist
            node_has_childs             = 3           " Node Has Subnodes
            node_is_aliased             = 4           " Still References to Node
            node_not_in_original_system = 5           " Current System Not Original System of Node
            transport_error             = 6           " Error in Transport Check/Insert for SICF Object
            tadir_error                 = 7           " TADIR Access Error
            db_error                    = 8           " Error while Writing to the Database
            error_occured               = 9           " Other Error
            OTHERS                      = 10
        ).
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
        DELETE FROM wdy_application WHERE application_name = <ls_wdy_application>-application_name.
        DELETE FROM wdy_applicationt WHERE application_name = <ls_wdy_application>-application_name.
        DELETE FROM wdy_app_property WHERE application_name = <ls_wdy_application>-application_name.
      ENDIF.
    ENDLOOP.

    INSERT wdy_config_data FROM TABLE lt_wdy_config_data.
    INSERT wdy_config_datt FROM TABLE lt_wdy_config_datt.
    INSERT wdy_config_compt FROM TABLE lt_wdy_config_compt.
    INSERT wdy_conf_delc FROM TABLE lt_wdy_conf_delc.
    INSERT wdy_config_appl FROM TABLE lt_wdy_config_appl.
    INSERT wdy_config_appt FROM TABLE lt_wdy_config_appt.
    INSERT wdy_conf_dela FROM TABLE lt_wdy_conf_dela.
    INSERT wdy_application FROM TABLE lt_wdy_application.
    INSERT wdy_applicationt FROM TABLE lt_wdy_applicationt.
    INSERT wdy_app_property FROM TABLE lt_wdy_app_property.

    COMMIT WORK.

    LOOP AT lt_wdy_application ASSIGNING <ls_wdy_application>.
      cl_wdy_md_application=>generate_sicf(
        EXPORTING
          p_applname             = <ls_wdy_application>-application_name             " Name of WDY Application
          p_devclass             = lv_devclass             " Package
          p_transport            = lv_trkorr            " Request/Task
          p_dark                 = abap_true                 " 'X'-> Without Popups (for FPM)
        EXCEPTIONS
          invalid_name           = 1                      " ICFNAME cannot be initial
          parent_not_existing    = 2                      " Parent Node Does Not Exist
          enqueue_error          = 3                      " Error in Database Enqueue
          node_already_existing  = 4                      " Node already exists
          transport_error        = 5                      " Error in Transport Check/Insert for SICF Object
          tadir_error            = 6                      " TADIR Access Error
          package_not_found      = 7                      " Specified Package Does Not Exist
          alternate_name_exist   = 8                      " Alternative Name Already Exists
          error_occured          = 9                      " Other Error
          error_on_create_admin  = 10                     " Error when creating the admin service
          OTHERS                 = 11
      ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
      DATA: p_object_type TYPE  wbobjtype.
      p_object_type-objtype_tr = 'WDYA'.
      p_object_type-subtype_wb = 'Y20'. " FPM appl.
      CALL FUNCTION 'WB_TREE_UPDATE_OBJECTLIST'
        EXPORTING
*         p_object_type       = swbm_c_type_wdy_application
          p_global_type       = p_object_type
          p_object_name       = <ls_wdy_application>-application_name
          p_operation         = swbm_c_op_insert
          p_package_name      = lv_devclass
          p_author            = sy-uname
        EXCEPTIONS
          error_occured       = 1
          invalid_operation   = 2
          no_objectlist_found = 3
          long_object_name    = 4
          OTHERS              = 5.
    ENDLOOP.

    LOOP AT lt_wdy_config_appl ASSIGNING <ls_wdy_config_appl>.
      MOVE-CORRESPONDING <ls_wdy_config_appl> TO ls_wdy_config_key.
      cl_wdr_cfg_persistence_utils=>config_changed(
        EXPORTING
          action              = if_wd_cfg_badi_changes=>co_action_create
          config_key          = ls_wdy_config_key          " Key Components of Configuration Tables
          devclass            = lv_devclass            " Package
          environment         = if_wd_cfg_badi_changes=>co_env_gui
          is_component        = abap_false
          object_name         = <ls_wdy_config_appl>-application         " Web Dynpro: Component Name
          pers_scope          = if_wd_personalization=>co_scope_config          " Web Dynpro: Personalization Range
          transport           = lv_trkorr           " Request/Task
      ).
    ENDLOOP.

    LOOP AT lt_wdy_config_data ASSIGNING <ls_wdy_config_data>.
      MOVE-CORRESPONDING <ls_wdy_config_data> TO ls_wdy_config_key.
      cl_wdr_cfg_persistence_utils=>config_changed(
        EXPORTING
          action              = if_wd_cfg_badi_changes=>co_action_create
          config_key          = ls_wdy_config_key          " Key Components of Configuration Tables
          devclass            = lv_devclass            " Package
          environment         = if_wd_cfg_badi_changes=>co_env_gui
          is_component        = abap_true
          object_name         = <ls_wdy_config_data>-component         " Web Dynpro: Component Name
          pers_scope          = if_wd_personalization=>co_scope_config          " Web Dynpro: Personalization Range
          transport           = lv_trkorr           " Request/Task
      ).
    ENDLOOP.


  ENDMETHOD.


  METHOD set_application.
    DATA: ls_wdca   TYPE wdy_config_appl,
          lv_string TYPE string.

    SELECT SINGLE *
      INTO ls_wdca
      FROM wdy_config_appl
      WHERE config_id = iv_wdca
        AND config_type = '02'
        AND config_var = ''
    .
    CHECK: sy-subrc EQ 0.

    ls_wdca-application = iv_appl.
    lv_string = cl_wdr_configuration_utils=>xml_xstring2string( in_xstring = ls_wdca-xcontent ).
    REPLACE REGEX '<Application Name="[^"]*"' IN lv_string WITH '<Application Name="' && iv_appl && '"'.
    REPLACE REGEX 'Usage="[^"]*"' IN lv_string WITH 'Usage="' && iv_appl && '"'.
    ls_wdca-xcontent = cl_wdr_configuration_utils=>xml_string2xstring( in_string = lv_string ).

    save_wdca( is_wdca = ls_wdca ).
  ENDMETHOD.


  METHOD set_description.

    corr_insert(
      EXPORTING
        iv_name     = iv_name
        iv_typekind = iv_typekind
    ).

    CASE iv_typekind.
      WHEN 'WDCA'.    " R3TR  WDCA  Web Dynpro Application Configuration
        DELETE FROM wdy_config_appt WHERE config_id = @iv_name AND config_type = '02' AND config_var = ''.
        IF iv_description IS NOT INITIAL.
          MODIFY wdy_config_appt FROM @( VALUE #( config_id = iv_name config_type = '02' langu = sy-langu description = iv_description ) ).
        ENDIF.
      WHEN 'WDCC'.    " R3TR  WDCC  Web Dynpro Component Configuration
        DELETE FROM wdy_config_datt WHERE config_id = @iv_name AND config_type = '00' AND config_var = ''.
        IF iv_description IS NOT INITIAL.
          MODIFY wdy_config_datt FROM @( VALUE #( config_id = iv_name config_type = '00' langu = sy-langu description = iv_description ) ).
        ENDIF.
      WHEN 'WDYA'.    " R3TR  WDYA  Web Dynpro Application
        DELETE FROM wdy_applicationt WHERE application_name = @iv_name.
        IF iv_description IS NOT INITIAL.
          MODIFY wdy_applicationt FROM @( VALUE #( application_name = iv_name langu = sy-langu description = iv_description ) ).
        ENDIF.
      WHEN 'WDYN'.    " R3TR  WDYN  Web Dynpro Component
        DELETE FROM wdy_componentt WHERE component_name = @iv_name.
        IF iv_description IS NOT INITIAL.
          MODIFY wdy_componentt FROM @( VALUE #( component_name = iv_name langu = sy-langu description = iv_description ) ).
        ENDIF.
      WHEN 'CLAS'.    " R3TR  CLAS  Class (ABAP Objects)
        DELETE FROM seoclasstx WHERE clsname = @iv_name.
        IF iv_description IS NOT INITIAL.
          MODIFY seoclasstx FROM @( VALUE #( clsname = iv_name langu = sy-langu descript = iv_description ) ).
        ENDIF.
      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.


  METHOD set_feeder_class.
    DATA: ls_wdcc         TYPE wdy_config_data,
          lo_translator	  TYPE REF TO if_wdr_config_otr,
          lv_string       TYPE string,
          lv_offset       TYPE i,
          lv_offset2      TYPE i,
          lv_length       TYPE i,
          lv_need_to_save TYPE flag.


    SELECT SINGLE *
      INTO ls_wdcc
      FROM wdy_config_data
      WHERE config_id = iv_config_id
        AND config_type = '00'
        AND config_var = ''
   .
    CHECK: sy-subrc EQ 0.

    lv_string = cl_wdr_configuration_utils=>xml_xstring2string( in_xstring = ls_wdcc-xcontent ).

    CASE ls_wdcc-component.
      WHEN if_fpm_cfg_constants=>gc_component_name-composite
        OR if_fpm_cfg_constants=>gc_component_name-tabbed
        OR if_fpm_cfg_constants=>gc_component_name-oif
        OR if_fpm_cfg_constants=>gc_component_name-gaf
        OR if_fpm_cfg_constants=>gc_component_name-ovp.
        " find appcc
        FIND '<Node Name="APP_SPECIFIC_CC"' IN lv_string MATCH OFFSET lv_offset.
        IF sy-subrc EQ 0.
          FIND '>' IN SECTION OFFSET lv_offset OF lv_string MATCH OFFSET lv_offset2.
          lv_offset2 = lv_offset2 - 1.
          IF lv_string+lv_offset2(1) EQ '/'.
            lv_offset2 = lv_offset2 + 2.
          ELSE.
            FIND '</Node>' IN SECTION OFFSET lv_offset2 OF lv_string MATCH OFFSET lv_offset2.
            lv_offset2 = lv_offset2 + 7.
          ENDIF.
        ELSE.
          FIND '</Item></Node></Item></Node></Component>' IN lv_string MATCH OFFSET lv_offset.
          lv_offset2 = lv_offset.
        ENDIF.
        lv_string = lv_string(lv_offset)
                 && |<Node Name="APP_SPECIFIC_CC" SimpleFormat="true"><Item Index="000001" SimpleFormat="true"><COMPONENT>{ iv_feeder_class }</COMPONENT></Item></Node>|
                 && lv_string+lv_offset2.
        lv_need_to_save = abap_true.

      WHEN OTHERS.
        " find feeder
        FIND REGEX '<FEEDER>[^<]*</FEEDER>' IN lv_string MATCH OFFSET lv_offset MATCH LENGTH lv_length.
        IF sy-subrc EQ 0.
          lv_offset = lv_offset + 8.
          lv_offset2 = lv_offset + lv_length - 17.
          lv_string = lv_string(lv_offset)
                   && iv_feeder_class
                   && lv_string+lv_offset2.
          lv_need_to_save = abap_true.
        ENDIF.
        " find param
        IF iv_set_feeder_param EQ abap_true.
          FIND '<Node Name="PARAMETER"' IN lv_string MATCH OFFSET lv_offset.
          IF sy-subrc EQ 0.
            FIND '>' IN SECTION OFFSET lv_offset OF lv_string MATCH OFFSET lv_offset2.
            lv_offset2 = lv_offset2 - 1.
            IF lv_string+lv_offset2(1) EQ '/'.
              lv_offset2 = lv_offset2 + 2.
            ELSE.
              FIND '</Node>' IN SECTION OFFSET lv_offset2 OF lv_string MATCH OFFSET lv_offset2.
              lv_offset2 = lv_offset2 + 7.
            ENDIF.
            lv_string = lv_string(lv_offset)
                     && |<Node Name="PARAMETER" SimpleFormat="true">{ iv_feeder_param }</Node>|
                     && lv_string+lv_offset2.
            lv_need_to_save = abap_true.
          ENDIF.
        ENDIF.

    ENDCASE.

    IF lv_need_to_save EQ abap_true.
      ls_wdcc-xcontent = cl_wdr_configuration_utils=>xml_string2xstring( in_string = lv_string ).
      save_wdcc( is_wdcc = ls_wdcc ).
    ENDIF.
  ENDMETHOD.


  METHOD suggest_object_name.
    DATA: lv_max_len TYPE i,
          lv_offset  TYPE i,
          lv_num     TYPE string,
          ls_cifkey	 TYPE seoclskey,
          lv_error   TYPE flag.

    rv_name = iv_name.

    CASE iv_typekind.
      WHEN 'WDCA'     " R3TR  WDCA  Web Dynpro Application Configuration
        OR 'WDCC'.    " R3TR  WDCC  Web Dynpro Component Configuration
        lv_max_len = 32.
      WHEN 'WDYA'     " R3TR  WDYA  Web Dynpro Application
        OR 'WDYN'     " R3TR  WDYN  Web Dynpro Component
        OR 'CLAS'.    " R3TR  CLAS  Class (ABAP Objects)
        lv_max_len = 30.
      WHEN OTHERS.
        lv_max_len = 30.
    ENDCASE.


    DO.
      IF sy-index EQ 1 AND iv_next_number EQ abap_false.
        " skip
      ELSE.
        " next number
        FIND REGEX '_\d+$' IN rv_name MATCH OFFSET lv_offset.
        IF sy-subrc EQ 0.
          lv_offset = lv_offset + 1.
          lv_num = rv_name+lv_offset.
          lv_num = lv_num + 1.
          lv_offset = lv_offset - 1.
          rv_name = rv_name(lv_offset).
        ELSE.
          lv_num = 1.
        ENDIF.
        CONDENSE lv_num.
        IF strlen( rv_name ) + 1 + strlen( lv_num ) > lv_max_len.
          lv_offset = lv_max_len - 1 - strlen( lv_num ).
          rv_name = rv_name(lv_offset) && '_' && lv_num.
        ELSE.
          rv_name = rv_name && '_' && lv_num.
        ENDIF.
      ENDIF.


      " check exist
      CASE iv_typekind.
        WHEN 'WDCA'.    " R3TR  WDCA  Web Dynpro Application Configuration
          SELECT SINGLE config_id
            INTO rv_name
            FROM wdy_config_appl
            WHERE config_id = rv_name
              AND config_type = '02'
              AND config_var = ''.
          IF sy-subrc <> 0.
            RETURN.
          ENDIF.
        WHEN 'WDCC'.    " R3TR  WDCC  Web Dynpro Component Configuration
          SELECT SINGLE config_id
            INTO rv_name
            FROM wdy_config_data
            WHERE config_id = rv_name
              AND config_type = '00'
              AND config_var = ''.
          IF sy-subrc <> 0.
            RETURN.
          ENDIF.
        WHEN 'WDYA'.    " R3TR  WDYA  Web Dynpro Application
          SELECT SINGLE application_name
            INTO rv_name
            FROM wdy_application
            WHERE application_name = rv_name.
          IF sy-subrc <> 0.
            RETURN.
          ENDIF.
        WHEN 'WDYN'.    " R3TR  WDYN  Web Dynpro Component
          SELECT SINGLE component_name
            INTO rv_name
            FROM wdy_component
            WHERE component_name = rv_name.
          IF sy-subrc <> 0.
            RETURN.
          ENDIF.
        WHEN 'CLAS'.    " R3TR  CLAS  Class (ABAP Objects)
          ls_cifkey-clsname = rv_name.
          cl_oo_abstract_class_tool=>check_clifname(
            CHANGING
              cifkey      = ls_cifkey
            EXCEPTIONS
              not_allowed = 1      " Objekttypname nicht zulässig
              OTHERS      = 2
          ).
          IF sy-subrc EQ 0.
            RETURN.
          ENDIF.
        WHEN OTHERS.
          cl_abap_datadescr=>describe_by_name(
            EXPORTING
              p_name         = rv_name      " Type name
*            RECEIVING
*              p_descr_ref    = p_descr_ref " Reference to description object
            EXCEPTIONS
              type_not_found = 1           " Type with name p_name could not be found
              OTHERS         = 2
          ).
          IF sy-subrc <> 0.
            RETURN.
          ENDIF.
      ENDCASE.
    ENDDO.

  ENDMETHOD.


  METHOD can_inherit.
    DATA:
      superclskey TYPE seoclskey,
      superclass  TYPE vseoclass.

    superclskey-clsname = iv_class.

* check superclass existence
    CALL FUNCTION 'SEO_CLASS_GET'
      EXPORTING
        clskey       = superclskey
        version      = seoc_version_inactive
        state        = '0'
      IMPORTING
*       SUPERCLASS   =
        class        = superclass
      EXCEPTIONS
        not_existing = 1
        deleted      = 2
        is_interface = 3
        model_only   = 4
        OTHERS       = 5.
    IF sy-subrc <> 0.
      IF iv_message EQ abap_true.
        MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
      RETURN.
    ENDIF.

* check superclass is final
    IF superclass-clsfinal = seox_true.
      IF iv_message EQ abap_true.
        MESSAGE e109(oo)
          WITH superclskey-clsname.
      ENDIF.
      RETURN.
    ENDIF.

    CASE superclass-category.
      WHEN seoc_category_persistent.
        IF iv_message EQ abap_true.
          MESSAGE e626(oo).
        ENDIF.
        RETURN.
      WHEN seoc_category_p_agent.
        IF iv_message EQ abap_true.
          MESSAGE e630(oo) WITH superclass-clsname.
        ENDIF.
        RETURN.
      WHEN seoc_category_exception.
        IF iv_message EQ abap_true.
          MESSAGE e192(oo).
        ENDIF.
        RETURN.
    ENDCASE.

    rv_can_inherit = abap_true.
  ENDMETHOD.


  METHOD save_wdca.
    DATA: ls_wdy_config_key TYPE wdy_config_key,
          lo_translator	    TYPE REF TO if_wdr_config_otr,
          lv_devclass	      TYPE devclass,
          lv_trkorr	        TYPE trkorr.


    ls_wdy_config_key-config_id = is_wdca-config_id.
    ls_wdy_config_key-config_type = is_wdca-config_type.
    ls_wdy_config_key-config_var = is_wdca-config_var.

    corr_insert(
      EXPORTING
        iv_name     = ls_wdy_config_key
        iv_typekind = 'WDCA'
      IMPORTING
        ev_devclass = lv_devclass
        ev_trkorr   = lv_trkorr
    ).

    MODIFY wdy_config_appl CONNECTION (if_wdr_cfg_constants=>c_db_con_name) FROM is_wdca.
    COMMIT CONNECTION (if_wdr_cfg_constants=>c_db_con_name).

    cl_wdr_cfg_persistence_utils=>config_changed(
      EXPORTING
        action              = if_wd_cfg_badi_changes=>co_action_modify
        config_key          = ls_wdy_config_key          " Key Components of Configuration Tables
        devclass            = lv_devclass            " Package
        environment         = if_wd_cfg_badi_changes=>co_env_gui
        is_component        = abap_false
        object_name         = is_wdca-application         " Web Dynpro: Component Name
        pers_scope          = if_wd_personalization=>co_scope_config          " Web Dynpro: Personalization Range
        transport           = lv_trkorr           " Request/Task
    ).


  ENDMETHOD.


  METHOD save_wdcc.
    DATA: ls_wdy_config_key TYPE wdy_config_key,
          lo_translator	    TYPE REF TO if_wdr_config_otr,
          lv_devclass	      TYPE devclass,
          lv_trkorr	        TYPE trkorr.


    ls_wdy_config_key-config_id = is_wdcc-config_id.
    ls_wdy_config_key-config_type = is_wdcc-config_type.
    ls_wdy_config_key-config_var = is_wdcc-config_var.

    corr_insert(
      EXPORTING
        iv_name     = ls_wdy_config_key
        iv_typekind = 'WDCC'
      IMPORTING
        ev_devclass = lv_devclass
        ev_trkorr   = lv_trkorr
    ).

    cl_wdr_cfg_persistence_utils=>save_comp_config_to_db(
      EXPORTING
        config_data = is_wdcc
        translator  = lo_translator
    ).

    cl_wdr_cfg_persistence_utils=>config_changed(
      EXPORTING
        action              = if_wd_cfg_badi_changes=>co_action_modify
        config_key          = ls_wdy_config_key          " Key Components of Configuration Tables
        devclass            = lv_devclass            " Package
        environment         = if_wd_cfg_badi_changes=>co_env_gui
        is_component        = abap_true
        object_name         = is_wdcc-component         " Web Dynpro: Component Name
        pers_scope          = if_wd_personalization=>co_scope_config          " Web Dynpro: Personalization Range
        transport           = lv_trkorr           " Request/Task
    ).
  ENDMETHOD.
ENDCLASS.
