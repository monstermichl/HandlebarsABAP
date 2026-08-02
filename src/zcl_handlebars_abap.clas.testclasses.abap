CLASS ltcl_handlebars_abap DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PUBLIC SECTION.
    CLASS-METHODS hello
      IMPORTING
        it_args          TYPE zcl_handlebars_abap=>tt_data
        is_options       TYPE zcl_handlebars_abap=>ts_options ##NEEDED
      RETURNING
        VALUE(rs_result) TYPE zcl_handlebars_abap=>ts_text_result.

    CLASS-METHODS arguments_check
      IMPORTING
        it_args          TYPE zcl_handlebars_abap=>tt_data
        is_options       TYPE zcl_handlebars_abap=>ts_options ##NEEDED
      RETURNING
        VALUE(rs_result) TYPE zcl_handlebars_abap=>ts_text_result.

    CLASS-METHODS inline_helper
      IMPORTING
        it_args          TYPE zcl_handlebars_abap=>tt_data
        is_options       TYPE zcl_handlebars_abap=>ts_options ##NEEDED
      RETURNING
        VALUE(rs_result) TYPE zcl_handlebars_abap=>ts_data_result.

  PRIVATE SECTION.
    TYPES: BEGIN OF ts_title,
             front TYPE string,
             back  TYPE string,
           END OF ts_title.

    TYPES: BEGIN OF ts_person,
             title     TYPE ts_title,
             firstName TYPE string,
             lastName  TYPE string,
           END OF ts_person.

    TYPES: tt_people TYPE STANDARD TABLE OF ts_person WITH EMPTY KEY.

    TYPES: BEGIN OF ts_manager,
             employees TYPE tt_people.
             INCLUDE TYPE ts_person.
    TYPES: END OF ts_manager.

    CONSTANTS: c_empty_error TYPE string VALUE ''.

    METHODS: template_standalone_success FOR TESTING.
    METHODS: template_structure_success FOR TESTING.
    METHODS: template_table_success FOR TESTING.
    METHODS: template_lookup_success FOR TESTING.
    METHODS: template_partial_hash_success FOR TESTING.
    METHODS: template_partial_success FOR TESTING.
    METHODS: template_partial_indnt_success FOR TESTING.
    METHODS: template_partial_ctx_success FOR TESTING.
    METHODS: template_partial_cmplx_success FOR TESTING.
    METHODS: template_args_check_success FOR TESTING.
    METHODS: template_inline_check_success FOR TESTING.
    METHODS: template_custom_helper_success FOR TESTING.
    METHODS: template_else_on_undef_success FOR TESTING.
    METHODS: template_resolve_order_success FOR TESTING.
    METHODS: template_load_template_fail FOR TESTING.

ENDCLASS.


CLASS ltcl_handlebars_abap IMPLEMENTATION.

  METHOD hello.
    ASSIGN it_args[ 1 ]->* TO FIELD-SYMBOL(<name>).
    DATA lv_title TYPE string.

    READ TABLE is_options-hashes WITH KEY key = 'title' INTO DATA(ls_key_value).

    IF ls_key_value IS NOT INITIAL.
      DATA ls_title TYPE ts_title.

      ASSIGN ls_key_value-data->* TO FIELD-SYMBOL(<title>).
      ls_title = CONV ts_title( <title> ).

      IF ls_title IS NOT INITIAL AND ls_title-front IS NOT INITIAL.
        lv_title = |{ ls_title-front } |.
      ENDIF.
    ENDIF.

    rs_result = is_options-instance->fn( NEW string( |Hello { lv_title }{ <name> } | ) ).
  ENDMETHOD.


  METHOD arguments_check.
    DATA lv_text TYPE string.

    LOOP AT it_args INTO DATA(lr_arg).
      ASSIGN lr_arg->* TO FIELD-SYMBOL(<arg>).
      lv_text = |{ lv_text } { <arg> }|.
    ENDLOOP.

    LOOP AT is_options-hashes INTO DATA(ls_hash).
      ASSIGN ls_hash-data->* TO FIELD-SYMBOL(<hash_value>).
      lv_text = |{ lv_text } { ls_hash-key }={ <hash_value> }|.
    ENDLOOP.

    CONDENSE lv_text.

    rs_result-text = lv_text.
  ENDMETHOD.


  METHOD inline_helper.
    DATA lv_text TYPE string.

    LOOP AT it_args INTO DATA(lr_arg).
      ASSIGN lr_arg->* TO FIELD-SYMBOL(<arg>).
      lv_text = |{ lv_text } { <arg> }|.
    ENDLOOP.

    LOOP AT is_options-hashes INTO DATA(ls_hash).
      ASSIGN ls_hash-data->* TO FIELD-SYMBOL(<hash_value>).
      lv_text = |{ lv_text } { ls_hash-key }={ <hash_value> }|.
    ENDLOOP.

    CONDENSE lv_text.

    rs_result-data = NEW string( lv_text ).
  ENDMETHOD.


  METHOD template_standalone_success.
    DATA(lv_newline) = cl_abap_char_utilities=>newline.
    DATA(ls_compile_result) = zcl_handlebars_abap=>compile(
      ` {{#if true}} `  && lv_newline &&
        'Hello World'   && lv_newline &&
      ` {{else}} `      && lv_newline &&
        'Ok, ciao'      && lv_newline &&
      ` {{/if}} `       && lv_newline
    ).
    cl_abap_unit_assert=>assert_equals( exp = c_empty_error act = ls_compile_result-error ).

    DATA(ls_template_result) = ls_compile_result-instance->template( ).

    cl_abap_unit_assert=>assert_equals( exp = c_empty_error act = ls_template_result-error ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'Hello World' && lv_newline
      act = ls_template_result-text
    ).
  ENDMETHOD.


  METHOD template_structure_success.
    DATA(ls_compile_result) = zcl_handlebars_abap=>compile(
      '{{! Simple comment }}' &
      '{{!-- Complex {{comment}} --}}' &
      '{{#with this.title as |title|}}' &
        '{{#if title.front}}' &
          '{{title.front}} ' &
        '{{/if}}' &
      '{{/with}}' &

      '{{firstName}} {{lastName}}'
    ).
    cl_abap_unit_assert=>assert_equals( exp = c_empty_error act = ls_compile_result-error ).

    DATA(lr_person) = VALUE ts_person(
      title = VALUE #( front = 'Ing.' back = 'BSc.' ) firstName = 'Peter' lastName = 'Parker'
    ).
    DATA(ls_template_result) = ls_compile_result-instance->template( lr_person ).

    cl_abap_unit_assert=>assert_equals( exp = c_empty_error act = ls_template_result-error ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'Ing. Peter Parker'
      act = ls_template_result-text
    ).
  ENDMETHOD.


  METHOD template_table_success.
    DATA(ls_compile_result) = zcl_handlebars_abap=>compile(
      '{{#each this as |row index|}}' &
        '{{#with row.title as |title|}}' &
          '{{#if title.front}}' &
            '{{title.front}}' &
          '{{else}}' &
            'Some' &
          '{{/if}}' &
        '{{/with}}' &

        ' {{firstName}} {{lastName}}' &
        '{{#unless index}}' &
          ', ' &
        '{{/unless}}' &
      '{{/each}}'
    ).
    cl_abap_unit_assert=>assert_equals( exp = c_empty_error act = ls_compile_result-error ).

    DATA(lr_people) = VALUE tt_people(
      ( title = VALUE #( back  = 'BSc.' ) firstName = 'Peter'  lastName  = 'Parker'  )
      ( title = VALUE #( front = 'Dr.'  ) firstName = 'Helene' lastName  = 'Fischer' )
    ).
    DATA(ls_template_result) = ls_compile_result-instance->template( lr_people ).

    cl_abap_unit_assert=>assert_equals( exp = c_empty_error act = ls_template_result-error ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'Some Peter Parker, Dr. Helene Fischer'
      act = ls_template_result-text
    ).
  ENDMETHOD.


  METHOD template_custom_helper_success.
    zcl_handlebars_abap=>register_helper_static( iv_name = 'hello' ir_helper = NEW zcl_handlebars_abap=>ts_class_helper( class_name = 'ltcl_handlebars_abap' method_name = 'hello' ) ).

    DATA(ls_compile_result) = zcl_handlebars_abap=>compile(
      '{{#each this as |person|}}' &
        '{{#hello person.firstName title=person.title}}' &
          '{{this}}' &
        '{{/hello}}' &
      '{{/each}}'
    ).
    cl_abap_unit_assert=>assert_equals( exp = c_empty_error act = ls_compile_result-error ).

    DATA(lr_people) = VALUE tt_people(
      ( firstName = 'Peter' title = VALUE #( front = 'Ing.' )  )
      ( firstName = 'Helene' )
    ).
    DATA(ls_template_result) = ls_compile_result-instance->template( lr_people ).

    cl_abap_unit_assert=>assert_equals( exp = c_empty_error act = ls_template_result-error ).

    CONDENSE ls_template_result-text.

    cl_abap_unit_assert=>assert_equals(
      exp = 'Hello Ing. Peter Hello Helene'
      act = ls_template_result-text
    ).
  ENDMETHOD.


  METHOD template_lookup_success.
    DATA(ls_employee) = VALUE ts_person(
      firstname = 'Marc'
      lastname = 'Cucurella'
    ).
    DATA(ls_manager) = VALUE ts_manager(
      firstname = 'Luis'
      lastname = 'de la Fuente'
      employees = VALUE #( ( ls_employee ) )
    ).
    DATA(ls_compile_result) = zcl_handlebars_abap=>compile(
        '{{lookup . "firstname"}} manages {{lookup (lookup employees 0) "firstname"}}'
    ).
    cl_abap_unit_assert=>assert_equals( exp = c_empty_error act = ls_compile_result-error ).

    DATA(ls_template_result) = ls_compile_result-instance->template( ls_manager ).

    cl_abap_unit_assert=>assert_equals( exp = c_empty_error act = ls_template_result-error ).

    CONDENSE ls_template_result-text.

    cl_abap_unit_assert=>assert_equals(
      exp = |{ ls_manager-firstname } manages { ls_employee-firstname }|
      act = ls_template_result-text
    ).
  ENDMETHOD.


  METHOD template_partial_hash_success.
    CONSTANTS c_title TYPE string VALUE 'Mr.'.

    zcl_handlebars_abap=>register_partial_static( iv_name = 'partial' iv_template_string = '{{title}} {{firstname}} {{lastname}} (manager: {{manager.firstname}} {{manager.lastname}})' ).

    DATA(ls_employee) = VALUE ts_person(
      firstname = 'Marc'
      lastname = 'Cucurella'
    ).
    DATA(ls_manager) = VALUE ts_manager(
      firstname = 'Luis'
      lastname = 'de la Fuente'
      employees = VALUE #( ( ls_employee ) )
    ).
    DATA(ls_compile_result) = zcl_handlebars_abap=>compile(
      '{{#each employees}}' &
        '{{> partial . title="' && c_title && '" manager=..}}' &
      '{{/each}}'
    ).
    cl_abap_unit_assert=>assert_equals( exp = c_empty_error act = ls_compile_result-error ).

    DATA(ls_template_result) = ls_compile_result-instance->template( ls_manager ).

    cl_abap_unit_assert=>assert_equals( exp = c_empty_error act = ls_template_result-error ).

    CONDENSE ls_template_result-text.

    cl_abap_unit_assert=>assert_equals(
      exp = |{ c_title } { ls_employee-firstname } { ls_employee-lastname } (manager: { ls_manager-firstname } { ls_manager-lastname })|
      act = ls_template_result-text
    ).
  ENDMETHOD.


  METHOD template_partial_success.
    zcl_handlebars_abap=>register_partial_static( iv_name = 'partial' iv_template_string = '{{title.front}} {{firstname}} {{lastname}}' ).

    DATA(ls_compile_result) = zcl_handlebars_abap=>compile(
      '{{> partial}}'
    ).
    cl_abap_unit_assert=>assert_equals( exp = c_empty_error act = ls_compile_result-error ).

    DATA(ls_employee) = VALUE ts_person(
      firstname = 'Marc'
      lastname = 'Cucurella'
    ).
    DATA(ls_template_result) = ls_compile_result-instance->template( ls_employee ).

    cl_abap_unit_assert=>assert_equals( exp = c_empty_error act = ls_template_result-error ).

    CONDENSE ls_template_result-text.

    cl_abap_unit_assert=>assert_equals(
      exp = |{ ls_employee-firstname } { ls_employee-lastname }|
      act = ls_template_result-text
    ).
  ENDMETHOD.


  METHOD template_partial_indnt_success.
    zcl_handlebars_abap=>register_partial_static( iv_name = 'partial' iv_template_string = '{{firstname}} {{lastname}}' ).

    DATA(lv_newline) = cl_abap_char_utilities=>newline.
    DATA(ls_compile_result) = zcl_handlebars_abap=>compile(
      `{{#each this}}`  && lv_newline &&
      `  {{> partial}}` && lv_newline &&
      ``                && lv_newline &&
      `{{/each}}`
    ).
    cl_abap_unit_assert=>assert_equals( exp = c_empty_error act = ls_compile_result-error ).

    DATA(lt_employees) = VALUE tt_people(
      (
        firstName = 'Marc'
        lastName  = 'Cucurella'
      )
      (
        firstName = 'Lamine'
        lastName  = 'Yamal'
      )
    ).
    DATA(ls_template_result) = ls_compile_result-instance->template( lt_employees ).

    cl_abap_unit_assert=>assert_equals( exp = c_empty_error act = ls_template_result-error ).

    DATA lv_expected TYPE string.

    LOOP AT lt_employees INTO DATA(ls_employee).
      lv_expected = |{ lv_expected }  { ls_employee-firstname } { ls_employee-lastname }\n|.
    ENDLOOP.

    cl_abap_unit_assert=>assert_equals(
      exp = lv_expected
      act = ls_template_result-text
    ).
  ENDMETHOD.


  METHOD template_partial_ctx_success.
    zcl_handlebars_abap=>register_partial_static( iv_name = 'partial' iv_template_string = '{{this}}' ).

    DATA(ls_compile_result) = zcl_handlebars_abap=>compile(
      '{{> partial "Marc"}}'
    ).
    cl_abap_unit_assert=>assert_equals( exp = c_empty_error act = ls_compile_result-error ).

    DATA(ls_template_result) = ls_compile_result-instance->template( ).

    cl_abap_unit_assert=>assert_equals( exp = c_empty_error act = ls_template_result-error ).

    CONDENSE ls_template_result-text.

    cl_abap_unit_assert=>assert_equals(
      exp = 'Marc'
      act = ls_template_result-text
    ).
  ENDMETHOD.


  METHOD template_partial_cmplx_success.
    DATA(lv_newline) = cl_abap_char_utilities=>newline.

    DATA(lv_error) = zcl_handlebars_abap=>register_partial_static(
      iv_name = 'formattedTitle'
      iv_template_string = '{{#if front}}{{front}} {{/if}}{{name}}{{#if back}}, {{back}}{{/if}}'
    ).
    cl_abap_unit_assert=>assert_equals( exp = c_empty_error act = lv_error ).

    lv_error = zcl_handlebars_abap=>register_partial_static(
      iv_name = 'personCard'
      iv_template_string =
        'Card: {{#unless manager}}MANAGER{{else}}EMPLOYEE{{/unless}}'                && lv_newline &&
        'Name: {{> formattedTitle front=title.front back=title.back name=lastName}}' && lv_newline &&
        'First name: {{firstName}}'                                                  && lv_newline &&
        'Role: {{role}}'                                                             && lv_newline &&
        '{{#if manager}}'                                                            && lv_newline &&
        'Manager: {{manager.firstName}} {{manager.lastName}}'                        && lv_newline &&
        '{{/if}}'
    ).
    cl_abap_unit_assert=>assert_equals( exp = c_empty_error act = lv_error ).

    DATA(ls_compile_result) = zcl_handlebars_abap=>compile(
      '=== MANAGER ==='                                  && lv_newline &&
      '{{> personCard . role="Head of Department"}}'     && lv_newline &&
      ''                                                 && lv_newline &&
      '=== EMPLOYEES ==='                                && lv_newline &&
      '{{#each employees}}'                              && lv_newline &&
      '{{> personCard . role="Team Member" manager=..}}' && lv_newline &&
      ''                                                 && lv_newline &&
      '{{/each}}'
    ).
    cl_abap_unit_assert=>assert_equals( exp = c_empty_error act = ls_compile_result-error ).

    DATA(ls_manager) = VALUE ts_manager(
      firstName = 'Luis'
      lastName  = 'de la Fuente'
      title     = VALUE #( front = 'Dr.' back = 'PhD' )
      employees = VALUE #(
        (
          firstName = 'Marc'
          lastName  = 'Cucurella'
          title     = VALUE #( front = 'B.Sc.' )
        )
        (
          firstName = 'Lamine'
          lastName  = 'Yamal'
        )
      )
    ).
    DATA(ls_template_result) = ls_compile_result-instance->template( ls_manager ).

    cl_abap_unit_assert=>assert_equals( exp = c_empty_error act = ls_template_result-error ).

    DATA(lv_expected) =
      '=== MANAGER ==='             && lv_newline &&
      'Card: MANAGER'               && lv_newline &&
      'Name: Dr. de la Fuente, PhD' && lv_newline &&
      'First name: Luis'            && lv_newline &&
      'Role: Head of Department'    && lv_newline &&
      ''                            && lv_newline &&
      '=== EMPLOYEES ==='           && lv_newline &&
      'Card: EMPLOYEE'              && lv_newline &&
      'Name: B.Sc. Cucurella'       && lv_newline &&
      'First name: Marc'            && lv_newline &&
      'Role: Team Member'           && lv_newline &&
      'Manager: Luis de la Fuente'  && lv_newline &&
      ''                            && lv_newline &&
      'Card: EMPLOYEE'              && lv_newline &&
      'Name: Yamal'                 && lv_newline &&
      'First name: Lamine'          && lv_newline &&
      'Role: Team Member'           && lv_newline &&
      'Manager: Luis de la Fuente'  && lv_newline &&
      ''                            && lv_newline.

    cl_abap_unit_assert=>assert_equals(
      exp = lv_expected
      act = ls_template_result-text
    ).
  ENDMETHOD.


  METHOD template_args_check_success.
    zcl_handlebars_abap=>register_helper_static( iv_name = 'arguments_check' ir_helper = NEW zcl_handlebars_abap=>ts_class_helper( class_name = 'ltcl_handlebars_abap' method_name = 'arguments_check' ) ).

    DATA(ls_compile_result) = zcl_handlebars_abap=>compile(
      '{{#arguments_check "literal" 123 true false null undefined s="s" i=123 b=true u=undefined n=null}}' &
      '{{/arguments_check}}'
    ).
    cl_abap_unit_assert=>assert_equals( exp = c_empty_error act = ls_compile_result-error ).

    DATA(ls_template_result) = ls_compile_result-instance->template( ).

    cl_abap_unit_assert=>assert_equals( exp = c_empty_error act = ls_template_result-error ).

    CONDENSE ls_template_result-text.

    cl_abap_unit_assert=>assert_equals(
      exp = 'literal 123 X s=s i=123 b=X u= n='
      act = ls_template_result-text
    ).
  ENDMETHOD.


  METHOD template_inline_check_success.
    zcl_handlebars_abap=>register_helper_static( iv_name = 'arguments_check' ir_helper = NEW zcl_handlebars_abap=>ts_class_helper( class_name = 'ltcl_handlebars_abap' method_name = 'arguments_check' ) ).
    zcl_handlebars_abap=>register_helper_static( iv_name = 'inline_helper'   ir_helper = NEW zcl_handlebars_abap=>ts_class_helper( class_name = 'ltcl_handlebars_abap' method_name = 'inline_helper'   ) ).

    DATA(ls_compile_result) = zcl_handlebars_abap=>compile(
      '{{#arguments_check (inline_helper true s="literal")}}' &
      '{{/arguments_check}}'
    ).
    cl_abap_unit_assert=>assert_equals( exp = c_empty_error act = ls_compile_result-error ).

    DATA(ls_template_result) = ls_compile_result-instance->template( ).

    cl_abap_unit_assert=>assert_equals( exp = c_empty_error act = ls_template_result-error ).

    CONDENSE ls_template_result-text.

    cl_abap_unit_assert=>assert_equals(
      exp = 'X s=literal'
      act = ls_template_result-text
    ).
  ENDMETHOD.


  METHOD template_else_on_undef_success.
    CONSTANTS c_success TYPE string VALUE 'success'.

    cl_abap_unit_assert=>assert_equals(
      exp = c_success
      act = zcl_handlebars_abap=>compile( '{{#if firstName}}{{else}}' && c_success && '{{/if}}' )-instance->template( )-text
    ).

    cl_abap_unit_assert=>assert_equals(
      exp = c_success
      act = zcl_handlebars_abap=>compile( '{{#if person.firstName}}{{else}}' && c_success && '{{/if}}' )-instance->template( )-text
    ).

    cl_abap_unit_assert=>assert_equals(
      exp = c_success
      act = zcl_handlebars_abap=>compile( '{{#if ../firstName}}{{else}}' && c_success && '{{/if}}' )-instance->template( )-text
    ).
  ENDMETHOD.


  METHOD template_resolve_order_success.
    TYPES: BEGIN OF ts_superior,
             title     TYPE ts_title,
             firstName TYPE string,
             lastName  TYPE string,
             employee  TYPE ts_person,
           END OF ts_superior.

    TYPES: BEGIN OF ty_address,
             city TYPE string,
           END OF ty_address.

    TYPES: BEGIN OF ty_pet,
             name TYPE string,
           END OF ty_pet.

    TYPES: tt_pets TYPE STANDARD TABLE OF ty_pet WITH EMPTY KEY.

    TYPES: BEGIN OF ty_user,
             name    TYPE string,
             address TYPE ty_address,
             pets    TYPE tt_pets,
           END OF ty_user.

    TYPES: BEGIN OF ty_item,
             name TYPE string,
           END OF ty_item.

    TYPES: tt_items TYPE STANDARD TABLE OF ty_item WITH EMPTY KEY.

    TYPES: BEGIN OF ty_root,
             title TYPE string,
             user  TYPE ty_user,
             items TYPE tt_items,
           END OF ty_root.

    DATA(lv_template_part_1) = '{{title}}' &
      '{{#with user as |u|}}' &
      '{{u.name}}' &
      '{{name}}' &
      '{{u.address.city}}'.

    DATA(lv_template_part_2) = '{{#each u.pets as |pet i|}}' &
      '{{pet.name}}' &
      '{{../name}}' &
      '{{../../title}}' &
    '{{/each}}' &
  '{{/with}}'.

    DATA(lv_template_part_3) = 'List of items:' &
    '{{#each items as |item i|}}' &
      '{{name}}' &
      '{{../this.title}}' &
      '{{i}}' &
    '{{/each}}'.

    DATA(lv_template) = lv_template_part_1 && lv_template_part_2 && lv_template_part_3.
    DATA(ls_root) = VALUE ty_root(
      title = 'Demo Root Title'
      user  = VALUE ty_user(
        name    = 'Alice'
        address = VALUE ty_address( city = 'Berlin' )
        pets    = VALUE tt_pets(
          ( name = 'Fluffy' )
          ( name = 'Milo'   )
        )
      )
      items = VALUE tt_items(
        ( name = 'Alpha' )
        ( name = 'Beta'  )
        ( name = 'Gamma' )
      )
    ).

    DATA(ls_compile_result) = zcl_handlebars_abap=>compile( lv_template ).
    cl_abap_unit_assert=>assert_equals( exp = c_empty_error act = ls_compile_result-error ).

    DATA(ls_template_result) = ls_compile_result-instance->template( ls_root ).
    cl_abap_unit_assert=>assert_equals( exp = c_empty_error act = ls_template_result-error ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'Demo Root TitleAliceAliceBerlinFluffyAliceDemo Root TitleMiloAliceDemo Root TitleList of items:AlphaDemo Root Title0BetaDemo Root Title1GammaDemo Root Title2'
      act = ls_template_result-text
    ).
  ENDMETHOD.


  METHOD template_load_template_fail.
    DATA(ls_compile_result) = zcl_handlebars_abap=>compile( 'template_name' ).
    DATA(ls_template_result) = ls_compile_result-instance->template( ).

    cl_abap_unit_assert=>assert_equals( exp = 'template_name' act = ls_template_result-text ).
  ENDMETHOD.

ENDCLASS.
