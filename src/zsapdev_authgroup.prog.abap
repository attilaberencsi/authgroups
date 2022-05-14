*&---------------------------------------------------------------------*
*& Report zsapdev_authgroup
*&---------------------------------------------------------------------*
*& Authorization Group Helper
*&    - List Authorization Groups (SE54 Quick Jump)
*&    - List Authorization Groups used in Table/View maintenance dialogs (Custom Feature)
*&    - List Authorizations Profiles / PFCG Roles where a given Authorization Group is embedded (Custom Feature)
*&    - Maintain Authorization Groups (SE54 Quick Jump)
*&    - Assign Authorization Group to Table/View Maintenance Dialog (SE54 Quick Jump)
*&---------------------------------------------------------------------*
*& Author: Attila Berencsi, sapdev.eu
*& Version Info (YYMMDD): v220514
*& https://github.com/attilaberencsi/authgroups
*& Licence: MIT
*&---------------------------------------------------------------------*
*& Validated on ABAP 1909.
*&
*& Software Component  Release     Support Package       Support Package Level  Description
*& ========================================================================================================
*& S4FND               104         SAPK-10402INS4FND     0002                   Foundation
*& SAP_ABA             75E         SAPK-75E02INSAPABA    0002                   Cross-Application Component
*& SAP_BASIS           754         SAPK-75402INSAPBASIS  0002                   SAP Basis Component
*& SAP_GWFND           754         SAPK-75402INSAPGWFND  0002                   SAP Gateway Foundation
*& SAP_UI              754         SAPK-75404INSAPUI     0004                   User Interface Technology
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
      app->show_roles_with_group( ).

    WHEN p_assign."Assign Authorization Group to Table/View Maintenance
      app->submit_stddat_maintain( ).

  ENDCASE.
