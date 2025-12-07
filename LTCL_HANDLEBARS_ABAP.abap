CLASS ltc_handlebars_abap DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

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
ENDCLASS.


CLASS ltc_handlebars_abap IMPLEMENTATION.
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
    DATA(ls_template_result) = ls_compile_result-template->template( lr_person ).

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
    DATA(ls_template_result) = ls_compile_result-template->template( lr_people ).

    cl_abap_unit_assert=>assert_equals( exp = c_empty_error act = ls_template_result-error ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'Some Peter Parker, Dr. Helene Fischer'
      act = ls_template_result-text
    ).
  ENDMETHOD.
ENDCLASS.