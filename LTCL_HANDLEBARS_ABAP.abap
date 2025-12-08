CLASS ltcl_handlebars_abap DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PUBLIC SECTION.
    CLASS-METHODS hello
      IMPORTING
        io_instance      TYPE REF TO zcl_handlebars_abap
        iv_name          TYPE string
        it_args          TYPE zcl_handlebars_abap=>tt_data
        is_data          TYPE zcl_handlebars_abap=>ts_data
      RETURNING
        VALUE(rs_result) TYPE zcl_handlebars_abap=>ts_text_result.

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

    TYPES: tt_people TYPE STANDARD TABLE OF ts_person WITH DEFAULT KEY.

    CONSTANTS: c_empty_error TYPE string VALUE ''.

    DATA:
      f_Cut TYPE REF TO zcl_handlebars_abap.  "class under test

    METHODS: template_structure_success FOR TESTING.
    METHODS: template_table_success FOR TESTING.
    METHODS: template_custom_helper_success FOR TESTING.

ENDCLASS.


CLASS ltcl_handlebars_abap IMPLEMENTATION.
  METHOD hello.
    DATA lv_name TYPE string.
    lv_name = it_args[ 1 ]-this->*.

    rs_result = io_instance->fn( it_data = VALUE zcl_handlebars_abap=>tt_data(
      ( this = NEW string( |Hello { lv_name } | ) )
    ) ).
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
        '{{#hello person.firstName}}' &
          '{{this}}' &
        '{{/hello}}' &
      '{{/each}}'
    ).
    cl_abap_unit_assert=>assert_equals( exp = c_empty_error act = ls_compile_result-error ).

    DATA(lr_people) = VALUE tt_people(
      ( firstName = 'Peter'  )
      ( firstName = 'Helene' )
    ).
    DATA(ls_template_result) = ls_compile_result-instance->template( lr_people ).

    cl_abap_unit_assert=>assert_equals( exp = c_empty_error act = ls_template_result-error ).

    CONDENSE ls_template_result-text.

    cl_abap_unit_assert=>assert_equals(
      exp = 'Hello Peter Hello Helene'
      act = ls_template_result-text
    ).
  ENDMETHOD.
ENDCLASS.