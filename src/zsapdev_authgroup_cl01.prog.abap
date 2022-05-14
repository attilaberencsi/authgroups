*&---------------------------------------------------------------------*
*& Include zsapdev_authgroup_cl01
*&---------------------------------------------------------------------*
CLASS lcl_authgroup_app DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    "! Launch Authorization Groups Maintenance Report
    "! @parameter i_mode | s-display , u-update
    METHODS submit_stbrg_groups
      IMPORTING
        i_mode TYPE clike.

    "! Assign Authorization Group to Table/View Maintenance
    METHODS submit_stddat_maintain.

    "! Authorization Groups Used in Table/View Maintenance Dialogs
    METHODS show_authgroups_in_use.

ENDCLASS.

CLASS lcl_authgroup_app IMPLEMENTATION.

  METHOD submit_stbrg_groups.
    SUBMIT stbrg_groups
      WITH p_obj = 'S_TABU_DIS'
      WITH p_mode = i_mode
    AND RETURN.
  ENDMETHOD.

  METHOD submit_stddat_maintain.
    SUBMIT stddat_maintain
      WITH p_dispm = 'R'
      WITH p_mode = 'U'
    VIA SELECTION-SCREEN
    AND RETURN.
  ENDMETHOD.

  METHOD show_authgroups_in_use.

    DATA:
      maint_auth_groups TYPE STANDARD TABLE OF zsapdev_s_maint_authgrps.


    "Auth.Group usages
    SELECT DISTINCT tabname, cclass AS brgru FROM tddat
      INTO CORRESPONDING FIELDS OF TABLE @maint_auth_groups
      WHERE cclass IN @s_grp
        AND cclass <> ''
      ORDER BY tabname, cclass.

    CHECK maint_auth_groups IS NOT INITIAL.

    "Auth.Group details
    DATA(auth_groups) = cl_tbrg_auth=>get_value_list( id_object = c_auth_obj_table_maint ).

    SORT auth_groups BY brgru.

    "Table / View Texts
    SELECT tabname, tabclass, ddtext FROM dd02v INTO TABLE @DATA(tab_hdrs)
      FOR ALL ENTRIES IN @maint_auth_groups
        WHERE tabname = @maint_auth_groups-tabname
          AND actflag = @space
          AND ddlanguage = @sy-langu.

    SORT tab_hdrs BY tabname.

    SELECT viewname, ddtext FROM dd25v INTO TABLE @DATA(view_hdrs)
      FOR ALL ENTRIES IN @maint_auth_groups
        WHERE viewname = @maint_auth_groups-tabname
          AND as4local = 'A'
          AND ddlanguage = @sy-langu.

    SORT view_hdrs BY viewname.

    "Merge result list
    LOOP AT maint_auth_groups ASSIGNING FIELD-SYMBOL(<maint_auth_group>).

      READ TABLE auth_groups INTO DATA(auth_group)
        WITH KEY brgru = <maint_auth_group>-brgru
        BINARY SEARCH.

      IF sy-subrc = 0.
        MOVE-CORRESPONDING auth_group TO <maint_auth_group>.

        READ TABLE tab_hdrs INTO DATA(tab_hdr)
          WITH KEY
            tabname = <maint_auth_group>-tabname
          BINARY SEARCH.

        IF sy-subrc = 0.

          CASE tab_hdr-tabclass.
            WHEN 'TRANSP'.
              <maint_auth_group>-ddtext = tab_hdr-ddtext.

            WHEN 'VIEW'.
              READ TABLE view_hdrs INTO DATA(view_hdr)
                WITH KEY
                  viewname = <maint_auth_group>-tabname
                BINARY SEARCH.

              IF sy-subrc = 0.
                <maint_auth_group>-ddtext = view_hdr-ddtext.
              ENDIF.
          ENDCASE.
        ENDIF.
      ENDIF.

    ENDLOOP.

    "Adjust Field Catalog to hide unnecessary fields
    DATA fcat_grp_in_use TYPE slis_t_fieldcat_alv.

    CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = 'ZSAPDEV_S_MAINT_AUTHGRPS'
      CHANGING
        ct_fieldcat            = fcat_grp_in_use
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    LOOP AT fcat_grp_in_use ASSIGNING FIELD-SYMBOL(<field_meta>).
      CASE <field_meta>-fieldname.
        WHEN 'OBJECT' OR 'NO_HEADER_REF'.
          <field_meta>-no_out = abap_true.
      ENDCASE.
    ENDLOOP.

    "Show List of Auth.Groups in use with table/view name
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_structure_name = 'ZSAPDEV_S_MAINT_AUTHGRPS'
        it_fieldcat      = fcat_grp_in_use
        is_layout        = VALUE slis_layout_alv( zebra = abap_true colwidth_optimize = abap_true cell_merge = 'N' )
        i_grid_title     = CONV lvc_title( 'Authorization Group Usage in Maintenance Dialogs' )  "#EC NOTEXT
      TABLES
        t_outtab         = maint_auth_groups
      EXCEPTIONS
        program_error    = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
