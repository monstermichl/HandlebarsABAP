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

    DATA:
      f_Cut TYPE REF TO zcl_handlebars_abap.  "class under test

    METHODS: template_structure_success FOR TESTING.
    METHODS: template_table_success FOR TESTING.
ENDCLASS.


CLASS ltc_handlebars_abap IMPLEMENTATION.
  METHOD template_structure_success.
    DATA(ls_compile_result) = zcl_handlebars_abap=>compile(
      '{{#with this.title as |title|}}' &
        '{{#if title.front}}' &
          '{{title.front}} ' &
        '{{/if}}' &
      '{{/with}}' &

      '{{firstName}} {{lastName}}'
    ).
    DATA(lr_person) = NEW ts_person(
      title = VALUE #( front = 'Ing.' back = 'BSc.' ) firstName = 'Peter' lastName = 'Parker'
    ).
    DATA(ls_template_result) = ls_compile_result-template->template( lr_person ).

    cl_abap_unit_assert=>assert_equals( exp = '' act = ls_template_result-error ).
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
            '{{title.front}} ' &
          '{{else}}' &
          '{{/if}}' &
        '{{/with}}' &

        '{{firstName}} {{lastName}}' &
        '{{#unless index}}' &
          ', ' &
        '{{/unless}}' &
      '{{/each}}'
    ).
    DATA(lr_people) = NEW tt_people(
      ( title = VALUE #( back  = 'BSc.' ) firstName = 'Peter'  lastName  = 'Parker'  )
      ( title = VALUE #( front = 'Dr.'  ) firstName = 'Helene' lastName  = 'Fischer' )
    ).
    DATA(ls_template_result) = ls_compile_result-template->template( lr_people ).

    cl_abap_unit_assert=>assert_equals( exp = '' act = ls_template_result-error ).
    cl_abap_unit_assert=>assert_equals(
      exp = 'Peter Parker, Dr. Helene Fischer'
      act = ls_template_result-text
    ).
  ENDMETHOD.
ENDCLASS.