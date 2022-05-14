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
  p_grprol TYPE  tddat-cclass.

PARAMETERS:
  p_edigrp RADIOBUTTON GROUP ro,
  p_assign RADIOBUTTON GROUP ro.

INCLUDE zsapdev_authgroup_cl01.


START-OF-SELECTION.

  DATA(app) = NEW lcl_authgroup_app( ).

  CASE abap_true.

    WHEN p_allgrp."Display List of Authorization Groups
      app->submit_stbrg_groups( i_mode = 'S' ). "display mode

    WHEN p_edigrp. "Maintain Authorization Groups
      app->submit_stbrg_groups( i_mode = 'U' ). "update mode

    WHEN p_usegrp. "Authorization Groups Used in Table/View Maintenance Dialogs
      app->show_authgroups_in_use( ).

    WHEN p_roles."Show roles having Authorization Groups


    WHEN p_assign."Assign Authorization Group to Table/View Maintenance
      app->submit_stddat_maintain( ).

  ENDCASE.
