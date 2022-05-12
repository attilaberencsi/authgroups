*&---------------------------------------------------------------------*
*& Report zsapdev_authgroup
*&---------------------------------------------------------------------*
*& Authorization Group Helper
*&    - List Authorization Groups
*&    - List Authorization Groups in use to protect table maintenance dialogs
*&    - List Authorizations Profiles / PFCG Roles where a given Authorization group is embedded
*&    - Maintain Authorization Groups
*&    - Assign Authorization Group to Table/View Maintenance Dialog
*&---------------------------------------------------------------------*
REPORT zsapdev_authgroup.

TABLES: tddat.

CONSTANTS:
  c_auth_obj_table_maint  TYPE xuobject VALUE 'S_TABU_DIS'.

PARAMETERS:
  p_allgrp RADIOBUTTON GROUP ro,
  p_usegrp RADIOBUTTON GROUP ro.

SELECT-OPTIONS:
  s_grp    FOR tddat-cclass.

PARAMETERS:
  p_roles  RADIOBUTTON GROUP ro,
  p_edigrp RADIOBUTTON GROUP ro,
  p_assign RADIOBUTTON GROUP ro.

DATA:
  maint_auth_groups TYPE STANDARD TABLE OF zsapdev_s_maint_authgrps.

START-OF-SELECTION.

  CASE abap_true.

    WHEN p_allgrp."Display List of Authorization Groups

      SUBMIT stbrg_groups
        WITH p_obj = 'S_TABU_DIS'
        WITH p_mode = 'S' "display mode
      AND RETURN.


    WHEN p_edigrp. "Maintain Authorization Groups

      SUBMIT stbrg_groups
        WITH p_obj = 'S_TABU_DIS'
        WITH p_mode = 'U' "update mode
      AND RETURN.


    WHEN p_usegrp. "Authorization Groups Used in Table/View Maintenance Dialogs

      "Auth.Group usages
      SELECT DISTINCT tabname, cclass AS brgru FROM tddat
        INTO CORRESPONDING FIELDS OF TABLE @maint_auth_groups
        WHERE cclass IN @s_grp
          AND cclass <> ''
        ORDER BY tabname, cclass.

      "Auth.Group details
      DATA(auth_groups) = cl_tbrg_auth=>get_value_list( id_object = c_auth_obj_table_maint ).

      SORT auth_groups BY brgru.

      "Merge result list
      LOOP AT maint_auth_groups ASSIGNING FIELD-SYMBOL(<maint_auth_group>).

        READ TABLE auth_groups INTO DATA(auth_group)
          WITH KEY brgru = <maint_auth_group>-brgru
          BINARY SEARCH.

        IF sy-subrc = 0.
          MOVE-CORRESPONDING auth_group TO <maint_auth_group>.
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


    WHEN p_assign."Assign Authorization Group to Table/View Maintenance

      SUBMIT stddat_maintain
        WITH p_dispm = 'R'
        WITH p_mode = 'U'
      VIA SELECTION-SCREEN
      AND RETURN.


  ENDCASE.
