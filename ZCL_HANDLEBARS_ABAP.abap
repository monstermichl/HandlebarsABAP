" HandlebarsABAP {{version}} (https://github.com/monstermichl/HandlebarsABAP)
CLASS zcl_handlebars_abap DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    TYPES: BEGIN OF ts_compile_result,
             instance TYPE REF TO zcl_handlebars_abap,
             error    TYPE string,
           END OF ts_compile_result.

    TYPES: BEGIN OF ts_template_result,
             text  TYPE string,
             error TYPE string,
           END OF ts_template_result.

    TYPES: BEGIN OF ts_text_result,
             text  TYPE string,
             error TYPE string,
           END OF ts_text_result.

    TYPES: BEGIN OF ts_class_helper,
             class_name  TYPE string,
             method_name TYPE string,
           END OF ts_class_helper.

    TYPES: BEGIN OF ts_object_helper,
             object      TYPE REF TO object,
             method_name TYPE string,
           END OF ts_object_helper.

    TYPES: BEGIN OF ts_func_module_helper,
             function_name TYPE string,
           END OF ts_func_module_helper.

    TYPES: BEGIN OF ts_form_helper,
             form_name   TYPE string,
             report_name TYPE string,
           END OF ts_form_helper.

    TYPES: BEGIN OF ts_helper,
             name   TYPE string,
             helper TYPE REF TO data,
           END OF ts_helper.

    TYPES: BEGIN OF ts_is_truthy_result,
             truthy TYPE abap_bool,
             error  TYPE string,
           END OF ts_is_truthy_result.

    TYPES: tr_data TYPE REF TO data.

    TYPES: tt_data TYPE STANDARD TABLE OF REF TO data WITH DEFAULT KEY.

    "! Compiles the passed Handlebars template.
    "!
    "! @parameter iv_template_string | Handebars template string.
    CLASS-METHODS compile
      IMPORTING
        iv_template_string TYPE string
      RETURNING
        VALUE(rs_result)   TYPE ts_compile_result.

    "! Registers a helper method globally. The passed method must implement the following signature.
    "!
    "! METHODS helper_method
    "!   IMPORTING
    "!     io_instance      TYPE zcl_handlebars_abap
    "!     iv_name          TYPE string
    "!     it_args          TYPE tt_data
    "!     is_data          TYPE REF TO data
    "!   RETURNING
    "!     VALUE(rs_result) TYPE ts_text_result.
    "!
    "! @parameter iv_name   | Helper name.
    "! @parameter ir_helper | Helper configuration (ts_class_helper | ts_object_helper | ts_func_module_helper | ts_form_helper).
    CLASS-METHODS register_helper_static
      IMPORTING
        iv_name         TYPE string
        ir_helper       TYPE any
      RETURNING
        VALUE(rv_error) TYPE string.

    "! Registers a helper method. The method must implement the following signature.
    "!
    "! METHODS helper_method
    "!   IMPORTING
    "!     io_instance      TYPE zcl_handlebars_abap
    "!     iv_name          TYPE string
    "!     it_args          TYPE tt_data
    "!     is_data          TYPE REF TO data
    "!   RETURNING
    "!     VALUE(rs_result) TYPE ts_text_result.
    "!
    "! @parameter iv_name   | Helper name.
    "! @parameter ir_helper | Helper configuration (ts_class_helper | ts_object_helper | ts_func_module_helper | ts_form_helper).
    METHODS register_helper
      IMPORTING
        iv_name         TYPE string
        ir_helper       TYPE any
      RETURNING
        VALUE(rv_error) TYPE string.

    "! Fills the compiled template with the passed data.
    "!
    "! @parameter ia_data   | A struct or table.
    METHODS template
      IMPORTING
        ia_data          TYPE any OPTIONAL
      RETURNING
        VALUE(rs_result) TYPE ts_template_result.

    "! Renders the current block's content.
    "!
    "! @parameter it_data | Data that shall be available within the block. The first entry is considered as 'this'.
    METHODS fn
      IMPORTING
        ia_data          TYPE any OPTIONAL
      RETURNING
        VALUE(rs_result) TYPE ts_text_result.

    "! Renders the current block's else-content.
    "!
    "! @parameter it_data | Data that shall be available within the block. The first entry is considered as 'this'.
    METHODS inverse
      IMPORTING
        ia_data          TYPE any OPTIONAL
      RETURNING
        VALUE(rs_result) TYPE ts_text_result.

    "! Creates an error message with added token information.
    "!
    "! @parameter iv_error       | Error string.
    "! @parameter iv_param_index | Optional argument to specify at which argument the error occurred.
    "! @parameter rv_error       | Enhanced error string.
    METHODS error
      IMPORTING
        iv_error        TYPE string
        iv_param_index  TYPE i OPTIONAL
      RETURNING
        VALUE(rv_error) TYPE string.

    "! Checks if the passed data can be considered truthy.
    "!
    "! @parameter ir_data | Data to check.
    METHODS is_truthy
      IMPORTING
        ir_data          TYPE REF TO data
      RETURNING
        VALUE(rs_result) TYPE ts_is_truthy_result.

  PROTECTED SECTION.

  PRIVATE SECTION.
    TYPES: tt_helpers TYPE TABLE OF ts_helper.

    TYPES: BEGIN OF ts_find_helper_result,
             helper TYPE REF TO ts_helper,
             error  TYPE string,
           END OF ts_find_helper_result.

    TYPES: BEGIN OF ts_get_data_type_result,
             name   TYPE string,
             is_ref TYPE abap_bool,
           END OF ts_get_data_type_result.

    CLASS-DATA: cr_helper_instance TYPE REF TO zcl_handlebars_abap.

    DATA: mt_helpers TYPE tt_helpers.

    CLASS-METHODS get_instance
      RETURNING
        VALUE(rr_instance) TYPE REF TO zcl_handlebars_abap.

    CLASS-METHODS register_helper_internal
      IMPORTING
        ir_instance     TYPE REF TO zcl_handlebars_abap
        iv_name         TYPE string
        ir_helper       TYPE any
      RETURNING
        VALUE(rv_error) TYPE string.

    CLASS-METHODS find_helper
      IMPORTING
        ir_instance      TYPE REF TO zcl_handlebars_abap
        iv_name          TYPE string
      RETURNING
        VALUE(rs_result) TYPE ts_find_helper_result.

    CLASS-METHODS get_data_type
      IMPORTING
        ia_data        TYPE any
      RETURNING
        VALUE(rs_type) TYPE ts_get_data_type_result.

    CLASS-METHODS any_to_ref_to_data
      IMPORTING
        ia_data        TYPE any
      RETURNING
        VALUE(rr_data) TYPE REF TO data.

    METHODS constructor
      IMPORTING
        iv_import_static_helpers TYPE abap_bool.

    " .:: Tokenizer section.
    TYPES: BEGIN OF ENUM e_tokenizer_token_types,
             e_token_type_unknown,
             e_token_type_text,
             e_token_type_hashtag,
             e_token_type_slash,
             e_token_type_o_round_bracket,
             e_token_type_c_round_bracket,
             e_token_type_pipe,
             e_token_type_at,
             e_token_type_else,
             e_token_type_as,
             e_token_type_null,
             e_token_type_undefined,
             e_token_type_bool_literal,
             e_token_type_number_literal,
             e_token_type_string_literal,
             e_token_type_path,
             e_token_type_space,
             e_token_type_eop,
             e_token_type_eof,
           END OF ENUM e_tokenizer_token_types.

    TYPES: tt_tokenizer_token_types TYPE STANDARD TABLE OF e_tokenizer_token_types WITH DEFAULT KEY.

    TYPES: BEGIN OF ts_tokenizer_placeholder,
             offset     TYPE i,
             length     TYPE i,
             content    TYPE string,
             is_comment TYPE abap_bool,
           END OF ts_tokenizer_placeholder.

    TYPES: tt_tokenizer_placeholders TYPE STANDARD TABLE OF ts_tokenizer_placeholder WITH DEFAULT KEY.

    TYPES: BEGIN OF ts_tokenizer_token,
             position TYPE i,
             value    TYPE string,
             type     TYPE e_tokenizer_token_types,
           END OF ts_tokenizer_token.

    TYPES: tt_tokenizer_tokens TYPE STANDARD TABLE OF ts_tokenizer_token WITH DEFAULT KEY.

    DATA: c_if       TYPE string VALUE 'if',
          c_unless   TYPE string VALUE 'unless',
          c_each     TYPE string VALUE 'each',
          c_with     TYPE string VALUE 'with',
          c_else     TYPE string VALUE 'else',
          c_true     TYPE string VALUE 'true',
          c_false    TYPE string VALUE 'false',
          c_this     TYPE string VALUE 'this',
          c_relative TYPE string VALUE '..'.

    DATA: mt_tokenizer_tokens TYPE tt_tokenizer_tokens.

    "! Separates the provided Handebars template string into tokens.
    "!
    "! @parameter iv_template_string | Handlebars template string.
    METHODS tokenizer_tokenize
      IMPORTING
        iv_template_string TYPE string
      RETURNING
        VALUE(rv_error)    TYPE string.

    METHODS tokenizer_eval_placeholders
      IMPORTING iv_template_string     TYPE string
      RETURNING
                VALUE(rt_placeholders) TYPE tt_tokenizer_placeholders.

    METHODS tokenizer_add_token
      IMPORTING
        VALUE(iv_value)    TYPE string
        VALUE(iv_position) TYPE i
        VALUE(iv_type)     TYPE e_tokenizer_token_types.

    " .:: Parser section.
    TYPES: tr_parser_statement TYPE REF TO data.

    TYPES: tt_parser_statements TYPE STANDARD TABLE OF tr_parser_statement WITH DEFAULT KEY.

    TYPES: tr_parser_expression TYPE tr_parser_statement.

    TYPES: tt_parser_expressions TYPE STANDARD TABLE OF tr_parser_expression WITH DEFAULT KEY.

    TYPES: BEGIN OF ts_parser_stmt_base,
             token TYPE ts_tokenizer_token,
           END OF ts_parser_stmt_base.

    TYPES: BEGIN OF ts_parser_text,
             value TYPE string.
             INCLUDE TYPE ts_parser_stmt_base.
    TYPES: END OF ts_parser_text.

    TYPES: BEGIN OF ts_parser_bool_literal,
             value TYPE abap_bool.
             INCLUDE TYPE ts_parser_stmt_base.
    TYPES: END OF ts_parser_bool_literal.

    TYPES: BEGIN OF ts_parser_float_literal,
             value TYPE float.
             INCLUDE TYPE ts_parser_stmt_base.
    TYPES: END OF ts_parser_float_literal.

    TYPES: BEGIN OF ts_parser_string_literal,
             value TYPE string.
             INCLUDE TYPE ts_parser_stmt_base.
    TYPES: END OF ts_parser_string_literal.

    TYPES: BEGIN OF ts_parser_null_literal.
             INCLUDE TYPE ts_parser_stmt_base.
    TYPES: END OF ts_parser_null_literal.

    TYPES: BEGIN OF ts_parser_undefined_literal.
             INCLUDE TYPE ts_parser_stmt_base.
    TYPES: END OF ts_parser_undefined_literal.

    TYPES: BEGIN OF ts_parser_path,
             parts         TYPE string_table,
             is_identifier TYPE abap_bool.
             INCLUDE TYPE ts_parser_stmt_base.
    TYPES: END OF ts_parser_path.

    TYPES: BEGIN OF ts_parser_sub_expr,
             expr TYPE tr_parser_expression.
             INCLUDE TYPE ts_parser_stmt_base.
    TYPES: END OF ts_parser_sub_expr.

    TYPES: BEGIN OF ts_parser_body,
             statements TYPE tt_parser_statements.
             INCLUDE    TYPE ts_parser_stmt_base.
    TYPES: END OF ts_parser_body.

    TYPES: BEGIN OF ts_parser_template,
             body TYPE ts_parser_body.
             INCLUDE TYPE ts_parser_stmt_base.
    TYPES: END OF ts_parser_template.

    TYPES: BEGIN OF ts_parser_helper,
             name TYPE string,
             args TYPE tt_parser_expressions.
             INCLUDE TYPE ts_parser_stmt_base.
    TYPES: END OF ts_parser_helper.

    TYPES: BEGIN OF ts_parser_block_param,
             name TYPE string.
             INCLUDE    TYPE ts_parser_stmt_base.
    TYPES: END OF ts_parser_block_param.

    TYPES: tt_parser_block_params TYPE STANDARD TABLE OF ts_parser_block_param WITH DEFAULT KEY.

    TYPES: BEGIN OF ts_parser_block,
             body   TYPE ts_parser_body,
             else   TYPE ts_parser_body,
             params TYPE tt_parser_block_params.
             INCLUDE TYPE ts_parser_helper.
    TYPES:END OF ts_parser_block.

    TYPES: BEGIN OF ts_parser_inline_helper.
             INCLUDE TYPE ts_parser_helper.
    TYPES:END OF ts_parser_inline_helper.

    TYPES: BEGIN OF ts_parser_eval_result,
             error TYPE string,
             stmt  TYPE tr_parser_statement,
           END OF ts_parser_eval_result.

    TYPES: BEGIN OF ts_parser_eval_results,
             error TYPE string,
             stmts TYPE tt_parser_statements,
           END OF ts_parser_eval_results.

    TYPES: BEGIN OF ts_parser_eval_helper_result,
             error TYPE string,
             name  TYPE string,
           END OF ts_parser_eval_helper_result.

    TYPES: BEGIN OF ts_parser_eval_args_result,
             error       TYPE string,
             expressions TYPE tt_parser_expressions,
           END OF ts_parser_eval_args_result.

    DATA: mv_parser_index TYPE i,
          mr_template     TYPE REF TO data.

    "! Creates an AST by putting the parsed tokens into context.
    METHODS parser_parse
      RETURNING
        VALUE(rv_error) TYPE string.

    METHODS parser_build_error
      IMPORTING
        iv_error        TYPE string
        is_token        TYPE ts_tokenizer_token
      RETURNING
        VALUE(rv_error) TYPE string.

    METHODS parser_build_expected_error
      IMPORTING
        iv_error        TYPE string
        is_token        TYPE ts_tokenizer_token
      RETURNING
        VALUE(rv_error) TYPE string.

    METHODS parser_peek_at
      IMPORTING
        iv_at           TYPE i
      RETURNING
        VALUE(rs_token) TYPE ts_tokenizer_token.

    METHODS parser_peek
      RETURNING
        VALUE(rs_token) TYPE ts_tokenizer_token.

    METHODS parser_eat
      RETURNING
        VALUE(rs_token) TYPE ts_tokenizer_token.

    METHODS parser_eval_stmt
      RETURNING
        VALUE(rs_result) TYPE ts_parser_eval_result.

    METHODS parser_eval_stmts
      IMPORTING
        it_termination_token_types TYPE tt_tokenizer_token_types OPTIONAL
      RETURNING
        VALUE(rs_results)          TYPE ts_parser_eval_results.

    METHODS parser_eval_template
      RETURNING
        VALUE(rs_result) TYPE ts_parser_eval_result.

    METHODS parser_eval_block
      RETURNING
        VALUE(rs_result) TYPE ts_parser_eval_result.

    METHODS parser_eval_inline_helper
      IMPORTING
        it_termination_token_types TYPE tt_tokenizer_token_types OPTIONAL
      RETURNING
        VALUE(rs_result)           TYPE ts_parser_eval_result.

    METHODS parser_eval_expr
      IMPORTING
        it_termination_token_types TYPE tt_tokenizer_token_types OPTIONAL
      RETURNING
        VALUE(rs_result)           TYPE ts_parser_eval_result.

    METHODS parser_eval_sub_expr
      RETURNING
        VALUE(rs_result) TYPE ts_parser_eval_result.

    METHODS parser_eval_path
      RETURNING
        VALUE(rs_result) TYPE ts_parser_eval_result.

    METHODS parser_eval_helper_name
      RETURNING
        VALUE(rs_result) TYPE ts_parser_eval_helper_result.

    METHODS parser_eval_args
      IMPORTING
        it_termination_token_types TYPE tt_tokenizer_token_types OPTIONAL
      RETURNING
        VALUE(rs_result)           TYPE ts_parser_eval_args_result.

    METHODS parser_check_eop
      IMPORTING
        lv_peek         TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(rv_error) TYPE string.

    " .:: Backend section
    TYPES: BEGIN OF ENUM e_backend_data_kinds,
             e_backend_data_kind_unknown,
             e_backend_data_kind_undefined,
             e_backend_data_kind_simple,
             e_backend_data_kind_struct,
             e_backend_data_kind_table,
           END OF ENUM e_backend_data_kinds.

    TYPES: BEGIN OF ts_backend_block_param,
             name TYPE string,
             data TYPE REF TO data,
           END OF ts_backend_block_param.

    TYPES: tt_backend_block_params TYPE TABLE OF ts_backend_block_param.

    TYPES: BEGIN OF ts_backend_eval_expr_result,
             data  TYPE REF TO data,
             kind  TYPE e_backend_data_kinds,
             error TYPE string,
           END OF ts_backend_eval_expr_result.

    TYPES: BEGIN OF ts_backend_path_eval_result,
             data  TYPE REF TO data,
             kind  TYPE e_backend_data_kinds,
             error TYPE string,
           END OF ts_backend_path_eval_result.

    TYPES: BEGIN OF ts_backend_block_arg,
             param TYPE ts_parser_block_param,
             data  TYPE REF TO data,
           END OF ts_backend_block_arg.

    TYPES: tt_backend_block_args TYPE STANDARD TABLE OF ts_backend_block_arg WITH DEFAULT KEY.

    TYPES: BEGIN OF ts_backend_block_stack_block,
             block TYPE REF TO ts_parser_block,
             args  TYPE tt_backend_block_args,
           END OF ts_backend_block_stack_block.

    TYPES: tt_backend_block_stack TYPE TABLE OF ts_backend_block_stack_block.

    DATA: mr_root_data             TYPE REF TO data,
          mt_backend_block_stack   TYPE tt_backend_block_stack,
          mv_backend_inline_helper TYPE ts_parser_inline_helper.

    METHODS backend_build_error
      IMPORTING
        iv_error        TYPE string
        is_token        TYPE ts_tokenizer_token
      RETURNING
        VALUE(rv_error) TYPE string.

    METHODS backend_n_args_helper_error
      IMPORTING
        iv_expected     TYPE i
        iv_actual       TYPE i
      RETURNING
        VALUE(rv_error) TYPE string.

    METHODS backend_eval_body
      IMPORTING
        ir_block         TYPE ts_parser_body
        is_data          TYPE REF TO data OPTIONAL
      RETURNING
        VALUE(rs_result) TYPE ts_text_result.

    METHODS backend_eval_stmt
      IMPORTING
        ir_stmt          TYPE REF TO data
        is_data          TYPE REF TO data
      RETURNING
        VALUE(rs_result) TYPE ts_text_result.

    METHODS backend_eval_expr
      IMPORTING
        ir_stmt          TYPE REF TO data
        is_data          TYPE REF TO data OPTIONAL
      RETURNING
        VALUE(rs_result) TYPE ts_backend_eval_expr_result.

    METHODS backend_eval_literal_expr
      IMPORTING
        ir_stmt          TYPE REF TO data
        is_data          TYPE REF TO data OPTIONAL
      RETURNING
        VALUE(rs_result) TYPE ts_backend_eval_expr_result.

    METHODS backend_eval_helper
      IMPORTING
        ir_helper        TYPE REF TO data
        is_data          TYPE REF TO data
      RETURNING
        VALUE(rs_result) TYPE ts_text_result.

    METHODS backend_eval_block
      IMPORTING
        ir_block         TYPE REF TO ts_parser_block
        is_data          TYPE REF TO data
      RETURNING
        VALUE(rs_result) TYPE ts_text_result.

    METHODS backend_eval_block_helper
      IMPORTING
        iv_property      TYPE string
        ia_data          TYPE any OPTIONAL
      RETURNING
        VALUE(rs_result) TYPE ts_text_result.

    METHODS backend_eval_cond_helper
      IMPORTING
        io_instance      TYPE REF TO zcl_handlebars_abap
        iv_name          TYPE string
        it_args          TYPE tt_data
        is_data          TYPE REF TO data
      RETURNING
        VALUE(rs_result) TYPE ts_text_result.

    METHODS backend_eval_each_helper
      IMPORTING
        io_instance      TYPE REF TO zcl_handlebars_abap
        iv_name          TYPE string
        it_args          TYPE tt_data
        is_data          TYPE REF TO data
      RETURNING
        VALUE(rs_result) TYPE ts_text_result.

    METHODS backend_eval_with_helper
      IMPORTING
        io_instance      TYPE REF TO zcl_handlebars_abap
        iv_name          TYPE string
        it_args          TYPE tt_data
        is_data          TYPE REF TO data
      RETURNING
        VALUE(rs_result) TYPE ts_text_result.

    METHODS backend_eval_inline_helper
      IMPORTING
        ir_inline_helper TYPE REF TO ts_parser_inline_helper
        is_data          TYPE REF TO data
      RETURNING
        VALUE(rs_result) TYPE ts_text_result.

    METHODS backend_eval_log_helper
      IMPORTING
        io_instance      TYPE REF TO zcl_handlebars_abap
        iv_name          TYPE string
        it_args          TYPE tt_data
        is_data          TYPE REF TO data
      RETURNING
        VALUE(rs_result) TYPE ts_text_result.

    METHODS backend_eval_sub_expr
      IMPORTING
        ir_sub_expr      TYPE REF TO ts_parser_sub_expr
        is_data          TYPE REF TO data
      RETURNING
        VALUE(rs_result) TYPE ts_backend_path_eval_result.

    METHODS backend_eval_path
      IMPORTING
        ir_path          TYPE REF TO ts_parser_path
        is_data          TYPE REF TO data
      RETURNING
        VALUE(rs_result) TYPE ts_backend_path_eval_result.

    METHODS backend_get_data_kind
      IMPORTING
        ir_data        TYPE REF TO data
      RETURNING
        VALUE(rv_kind) TYPE e_backend_data_kinds.

    METHODS backend_get_block
      IMPORTING
        iv_index        TYPE i
      RETURNING
        VALUE(rr_block) TYPE REF TO ts_backend_block_stack_block.

    METHODS backend_get_last_block
      RETURNING
        VALUE(rr_block) TYPE REF TO ts_backend_block_stack_block.

    METHODS backend_call_helper
      IMPORTING
        iv_name          TYPE string
        it_args          TYPE tt_data OPTIONAL
        is_data          TYPE REF TO data
      RETURNING
        VALUE(rs_result) TYPE ts_text_result.

    METHODS backend_get_token_property
      IMPORTING
        ir_struct       TYPE REF TO data
      RETURNING
        VALUE(rs_token) TYPE ts_tokenizer_token.

ENDCLASS.



CLASS zcl_handlebars_abap IMPLEMENTATION.

  METHOD compile.
    DATA(lv_template_string) = iv_template_string.
    DATA(lo_template) = NEW zcl_handlebars_abap( abap_true ).

    " First, try to load stored HTML template.
    IF strlen( lv_template_string ) <= 40.
      DATA: lt_html_table  TYPE swww_t_html_table,
            lt_merge_table TYPE swww_t_merge_table.

      DATA(lv_template_name) = CONV swww_t_template_name( lv_template_string ).

      CALL FUNCTION 'WWW_HTML_MERGER'
        EXPORTING
          template           = lv_template_name
        IMPORTING
          html_table         = lt_html_table
        CHANGING
          merge_table        = lt_merge_table
        EXCEPTIONS
          template_not_found = 1.

      " If loading the template was successful, replace iv_template_string with the string from the loaded template.
      IF sy-subrc = 0.
        CLEAR lv_template_string.

        LOOP AT lt_html_table INTO DATA(ls_html_row).
          lv_template_string = |{ lv_template_string }{ CONV string( ls_html_row-line ) }|.
        ENDLOOP.
      ENDIF.
    ENDIF.

    " Tokenize template string (disassemble it into usable chunks).
    DATA(lv_error) = lo_template->tokenizer_tokenize( lv_template_string ).

    IF lv_error IS NOT INITIAL.
      rs_result-error = lv_error.
      RETURN.
    ENDIF.

    " Parse tokens and build an AST.
    lv_error = lo_template->parser_parse( ).

    IF lv_error IS NOT INITIAL.
      rs_result-error = lv_error.
      RETURN.
    ENDIF.

    rs_result-instance = lo_template.
  ENDMETHOD.


  METHOD register_helper_static.
    zcl_handlebars_abap=>register_helper_internal(
      ir_instance = zcl_handlebars_abap=>get_instance( )
      iv_name     = iv_name
      ir_helper   = ir_helper
    ).
  ENDMETHOD.


  METHOD register_helper.
    zcl_handlebars_abap=>register_helper_internal(
      ir_instance = me
      iv_name     = iv_name
      ir_helper   = ir_helper
    ).
  ENDMETHOD.


  METHOD template.
    DATA(lr_data) = me->any_to_ref_to_data( ia_data ).

    me->mr_root_data = lr_data.

    DATA(ls_result) = me->backend_eval_stmt(
      ir_stmt = me->mr_template
      is_data = lr_data
    ).
    DATA(lv_error) = ls_result-error.

    IF lv_error IS NOT INITIAL.
      rs_result-error = lv_error.
      RETURN.
    ENDIF.

    rs_result-text = ls_result-text.
  ENDMETHOD.


  METHOD fn.
    rs_result = me->backend_eval_block_helper( iv_property = 'body' ia_data = ia_data ).
  ENDMETHOD.


  METHOD inverse.
    rs_result = me->backend_eval_block_helper( iv_property = 'else' ia_data = ia_data ).
  ENDMETHOD.


  METHOD error.
    DATA ls_helper TYPE ts_parser_helper.

    " If current context is an inline-helper, get its values...
    IF me->mv_backend_inline_helper IS NOT INITIAL.
      MOVE-CORRESPONDING me->mv_backend_inline_helper TO ls_helper.
    ELSE.
      " ...otherwise get the current block helper.
      DATA(lr_block) = me->backend_get_last_block( ).

      IF lr_block IS BOUND.
        MOVE-CORRESPONDING lr_block->block->* TO ls_helper.
      ENDIF.
    ENDIF.

    " If a helper has been found, values can be evaluated.
    IF ls_helper IS NOT INITIAL.
      DATA(lt_args) = ls_helper-args.
      DATA(ls_token) = ls_helper-token.

      " If param index is in a valid range, try to get token property.
      IF iv_param_index > 0 AND iv_param_index < lines( lt_args ).
        DATA(ls_arg) = lt_args[ iv_param_index ].
        GET REFERENCE OF ls_arg INTO DATA(lr_arg).

        ls_token = me->backend_get_token_property( ir_struct = lr_arg ).
      ENDIF.

      " Augment passed error with token position.
      rv_error = me->backend_build_error( iv_error = iv_error is_token = ls_token ).
    ENDIF.
  ENDMETHOD.


  METHOD get_instance.
    IF zcl_handlebars_abap=>cr_helper_instance IS NOT BOUND.
      zcl_handlebars_abap=>cr_helper_instance = NEW zcl_handlebars_abap( abap_false ).
    ENDIF.

    rr_instance = zcl_handlebars_abap=>cr_helper_instance.
  ENDMETHOD.


  METHOD register_helper_internal.
    IF iv_name IS INITIAL.
      rv_error = 'No helper name provided'.
      RETURN.
    ENDIF.

    DATA(lr_helper) = zcl_handlebars_abap=>any_to_ref_to_data( ir_helper ).
    DATA(ls_type) = zcl_handlebars_abap=>get_data_type( ir_helper ).
    DATA(lv_type_name) = ls_type-name.

    CASE lv_type_name.
      WHEN 'ts_class_helper'       OR
           'ts_object_helper'      OR
           'ts_func_module_helper' OR
           'ts_form_helper'.
        " Good cases, nothing to do.

      WHEN OTHERS.
        rv_error = |Unsupported helper type { lv_type_name }|.
        RETURN.
    ENDCASE.

    DATA(ls_find_helper_result) = zcl_handlebars_abap=>find_helper( ir_instance = ir_instance iv_name = iv_name ).

    " If a corresponding helper was found, update it.
    IF ls_find_helper_result-error IS INITIAL.
      ls_find_helper_result-helper->helper ?= lr_helper.
    ELSE.
      APPEND VALUE ts_helper( name = iv_name helper = lr_helper ) TO ir_instance->mt_helpers.
    ENDIF.
  ENDMETHOD.


  METHOD find_helper.
    " Try to get a registered helper by its name
    READ TABLE ir_instance->mt_helpers REFERENCE INTO DATA(lr_helper) WHERE name = iv_name.

    IF sy-subrc <> 0.
      rs_result-error = |No helper found for { iv_name }|.
      RETURN.
    ENDIF.

    rs_result-helper = lr_helper.
  ENDMETHOD.


  METHOD get_data_type.
    DATA(lo_desc) = cl_abap_typedescr=>describe_by_data( ia_data ).
    DATA(lv_is_ref) = abap_false.

    IF lo_desc->kind = lo_desc->kind_ref.
      lv_is_ref = abap_true.
      lo_desc = cl_abap_typedescr=>describe_by_data_ref( ia_data ).
    ENDIF.

    DATA(lv_type) = lo_desc->get_relative_name( ).
    TRANSLATE lv_type TO LOWER CASE.

    rs_type-is_ref = lv_is_ref.
    rs_type-name = lv_type.
  ENDMETHOD.


  METHOD any_to_ref_to_data.
    DATA(ls_type) = zcl_handlebars_abap=>get_data_type( ia_data ).

    " If passed data is not a reference, get its reference.
    IF ls_type-is_ref = abap_false.
      DATA(lo_type) = CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_data( ia_data ) ).

      CREATE DATA rr_data TYPE HANDLE lo_type.
      GET REFERENCE OF ia_data INTO rr_data.
    ELSE.
      rr_data ?= ia_data.
    ENDIF.
  ENDMETHOD.


  METHOD constructor.
    IF iv_import_static_helpers = abap_true.
      DATA(lr_helper_instance) = zcl_handlebars_abap=>get_instance( ).

      " Add globally registered helpers.
      LOOP AT lr_helper_instance->mt_helpers INTO DATA(ls_helper).
        me->register_helper( iv_name = ls_helper-name ir_helper = ls_helper-helper ).
      ENDLOOP.
    ENDIF.

    " Register default block-helpers.
    me->register_helper( iv_name = c_if     ir_helper = NEW ts_object_helper( object = me method_name = 'backend_eval_cond_helper' ) ).
    me->register_helper( iv_name = c_unless ir_helper = NEW ts_object_helper( object = me method_name = 'backend_eval_cond_helper' ) ).
    me->register_helper( iv_name = c_each   ir_helper = NEW ts_object_helper( object = me method_name = 'backend_eval_each_helper' ) ).
    me->register_helper( iv_name = c_with   ir_helper = NEW ts_object_helper( object = me method_name = 'backend_eval_with_helper' ) ).

    " Register default inline-helpers.
    me->register_helper( iv_name = 'log' ir_helper = NEW ts_object_helper( object = me method_name = 'backend_eval_log_helper' ) ).
  ENDMETHOD.


  METHOD tokenizer_tokenize.
    TYPES: BEGIN OF ts_tokenizer_token_mapping,
             pattern TYPE string,
             type    TYPE e_tokenizer_token_types,
           END OF ts_tokenizer_token_mapping.

    TYPES: tt_token_mappings TYPE STANDARD TABLE OF ts_tokenizer_token_mapping WITH DEFAULT KEY.

    TYPES: BEGIN OF ts_match_mapping,
             match TYPE match_result,
             type  TYPE e_tokenizer_token_types,
           END OF ts_match_mapping.

    CONSTANTS: c_space           TYPE string VALUE '\s+',
               c_hashtag         TYPE string VALUE '\#',
               c_slash           TYPE string VALUE '\/',
               c_o_round_bracket TYPE string VALUE '\(',
               c_c_round_bracket TYPE string VALUE '\)',
               c_pipe            TYPE string VALUE '\|',
               c_at              TYPE string VALUE '\@',
               c_as              TYPE string VALUE 'as',
               c_null            TYPE string VALUE 'null',
               c_undefined       TYPE string VALUE 'undefined',
               c_number_pattern  TYPE string VALUE '(-|\+)?\d+(.\d+)?',
               c_path_pattern_1  TYPE string VALUE '(\.\.\/)*\w+(\.\w+)*',
               c_path_pattern_2  TYPE string VALUE '(\.\.\/)',
               c_path_pattern_3  TYPE string VALUE '\.'.

    DATA: lv_previous_offset TYPE i VALUE 0,
          lv_text            TYPE string.

    DATA(lt_char_mappings) = VALUE tt_token_mappings(
      ( pattern = c_space           type = e_token_type_space           )
      ( pattern = c_hashtag         type = e_token_type_hashtag         )
      ( pattern = c_o_round_bracket type = e_token_type_o_round_bracket )
      ( pattern = c_c_round_bracket type = e_token_type_c_round_bracket )
      ( pattern = c_slash           type = e_token_type_slash           )
      ( pattern = c_at              type = e_token_type_at              )
      ( pattern = c_pipe            type = e_token_type_pipe            )
    ).

    DATA(lt_keyword_mappings) = VALUE tt_token_mappings(
      ( pattern = c_as             type = e_token_type_as             )
      ( pattern = c_else           type = e_token_type_else           )
      ( pattern = c_null           type = e_token_type_null           )
      ( pattern = c_undefined      type = e_token_type_undefined      )
      ( pattern = c_true           type = e_token_type_bool_literal   )
      ( pattern = c_false          type = e_token_type_bool_literal   )
      ( pattern = c_number_pattern type = e_token_type_number_literal )
      ( pattern = c_path_pattern_1 type = e_token_type_path           )
      ( pattern = c_path_pattern_2 type = e_token_type_path           )
      ( pattern = c_path_pattern_3 type = e_token_type_path           )
    ).

    DATA(lv_text_length) = 0.
    DATA(lt_placeholders) = me->tokenizer_eval_placeholders( iv_template_string ).

    LOOP AT lt_placeholders INTO DATA(ls_placeholder).
      DATA(lv_placeholder_offset) = ls_placeholder-offset.
      DATA(lv_placeholder_length) = ls_placeholder-length.

      lv_text_length = lv_placeholder_offset - lv_previous_offset.
      lv_text = iv_template_string+lv_previous_offset(lv_text_length).

      IF lv_text_length > 0.
        me->tokenizer_add_token( iv_value = lv_text iv_position = lv_previous_offset iv_type = e_token_type_text ).
      ENDIF.

      " Update previous offset.
      lv_previous_offset = lv_placeholder_offset + lv_placeholder_length.

      " If placeholder is comment, skip it.
      IF ls_placeholder-is_comment <> abap_false.
        CONTINUE.
      ENDIF.

      DATA(lv_content) = ls_placeholder-content.
      DATA(lv_i) = 0.
      DATA(lv_collecting_string) = abap_false.

      DATA: lv_prev_c(1)                TYPE c,
            lv_collecting_string_before TYPE abap_bool,
            lv_collected_string_start   TYPE i,
            lv_collected_string         TYPE string.

      CLEAR lv_prev_c.

      " Iterate string to find parts.
      DO.
        DATA(lv_c) = lv_content+lv_i(1).
        DATA(lv_subcontent) = lv_content+lv_i.

        " Handle string collection.
        IF lv_c = '"'.
          lv_collecting_string_before = lv_collecting_string.

          IF lv_collecting_string = abap_false.
            lv_collecting_string = abap_true.
            lv_collected_string_start = lv_i.
          ELSEIF lv_prev_c <> '\'.
            DATA(lv_collected_string_offset) = lv_collected_string_start + 1.

            lv_collected_string = substring( val = lv_content off = lv_collected_string_offset len = lv_i - lv_collected_string_offset ).
            lv_collecting_string = abap_false.
          ENDIF.
        ENDIF.

        " +1 because lv_i starts at 0.
        " +2 because placeholder starts with "{{".
        DATA(lv_token_position) = lv_placeholder_offset + lv_i + 1 + 2.

        " If collecting string has changed, handle string.
        IF lv_collecting_string_before <> lv_collecting_string.

          " If not collecting anymore, add string to tokens.
          IF lv_collecting_string = abap_false.
            me->tokenizer_add_token( iv_value = lv_collected_string iv_position = lv_token_position iv_type = e_token_type_string_literal ).
            lv_collecting_string_before = lv_collecting_string.
          ENDIF.
        ELSE.
          DATA: ls_match         TYPE match_result,
                ls_match_mapping TYPE ts_match_mapping.

          CLEAR ls_match_mapping.

          " Try to find characters.
          LOOP AT lt_char_mappings INTO DATA(ls_mapping).
            FIND PCRE |^({ ls_mapping-pattern })| IN lv_subcontent RESULTS ls_match.

            IF sy-subrc = 0.
              ls_match_mapping = VALUE #(
                match = ls_match
                type = ls_mapping-type
              ).
              EXIT.
            ENDIF.
          ENDLOOP.

          DATA lv_part TYPE string.

          " If nothing found yet, try to find keywords.
          IF ls_match_mapping IS INITIAL.
            LOOP AT lt_keyword_mappings INTO ls_mapping.
              DATA(lv_pattern) = |^({ ls_mapping-pattern })(?=\\W\|$)|.

              FIND PCRE lv_pattern IN lv_subcontent RESULTS ls_match.

              IF sy-subrc = 0.
                ls_match_mapping = VALUE #(
                  match = ls_match
                  type = ls_mapping-type
                ).
                EXIT.
              ENDIF.
            ENDLOOP.
          ENDIF.

          " If a match was found, handle it.
          IF ls_match_mapping IS NOT INITIAL.
            ls_match = ls_match_mapping-match.
            lv_part = lv_subcontent+ls_match-offset(ls_match-length).

            DATA(lv_type) = ls_mapping-type.

            " Skip spaces.
            IF lv_type <> e_token_type_space.
              me->tokenizer_add_token( iv_value = lv_part iv_position = lv_token_position iv_type = lv_type ).
            ENDIF.

            lv_i = lv_i + ( ls_match-length - 1 ).

            " If nothing found, cancel with error.
          ELSE.
            rv_error = |Unexpected character '{ lv_c }' at position { lv_token_position }|.
          ENDIF.
        ENDIF.

        lv_i = lv_i + 1.
        lv_prev_c = lv_c.

        IF lv_i >= strlen( lv_content ).
          EXIT.
        ENDIF.
      ENDDO.

      " Add EOP (end-of-placeholder) token at the end of a placeholder.
      me->tokenizer_add_token( iv_value = '' iv_position = lv_token_position iv_type = e_token_type_eop ).
    ENDLOOP.

    lv_text = iv_template_string+lv_previous_offset.
    lv_text_length = strlen( lv_text ).

    IF lv_text_length > 0.
      me->tokenizer_add_token( iv_value = lv_text iv_position = lv_previous_offset iv_type = e_token_type_text ).
    ENDIF.

    " Terminate with EOF-token.
    me->tokenizer_add_token( iv_value = '' iv_position = -1 iv_type = e_token_type_eof ).
  ENDMETHOD.


  METHOD tokenizer_eval_placeholders.
    TYPES: BEGIN OF ENUM e_comment_types,
             e_comment_type_none,
             e_comment_type_simple,
             e_comment_type_complex,
           END OF ENUM e_comment_types.

    CONSTANTS: c_opening_brackets TYPE string VALUE '{{',
               c_closing_brackets TYPE string VALUE '}}',
               c_minus_minus      TYPE string VALUE '--'.

    DATA: lv_previous_c   TYPE c,
          lv_comment_type TYPE e_comment_types,
          lv_four_chars   TYPE string.

    DATA(lv_i) = 0.
    DATA(lv_template_length) = strlen( iv_template_string ).
    DATA(lv_start_index) = 0.

    WHILE lv_i < lv_template_length.
      DATA(lv_c) = iv_template_string+lv_i(1).
      DATA(lv_current_two_chars) = |{ lv_previous_c }{ lv_c }|.

      " Check if placeholder start has not been found yet.
      IF lv_start_index = 0.
        IF lv_current_two_chars = c_opening_brackets.
          lv_start_index = lv_i + 1.
        ENDIF.

        " If placeholder starts with an exclamation mark it's a comment.
      ELSEIF lv_start_index = lv_i AND lv_c = '!'.
        lv_comment_type = e_comment_type_simple.
      ELSEIF lv_comment_type = e_comment_type_simple AND lv_four_chars = |{ c_opening_brackets }!-| AND lv_current_two_chars = c_minus_minus.
        lv_comment_type = e_comment_type_complex.
      ENDIF.

      lv_four_chars = |{ lv_four_chars }{ lv_c }|.

      " Make sure, lv_four_chars contains a maximum of 4 chars.
      IF strlen( lv_four_chars ) > 4.
        SHIFT lv_four_chars.
      ENDIF.

      lv_i = lv_i + 1.

      " Check end of placeholder.
      IF (
        ( lv_comment_type =  e_comment_type_complex AND lv_four_chars = |{ c_minus_minus }{ c_closing_brackets }| ) OR
        ( lv_comment_type <> e_comment_type_complex AND lv_current_two_chars = c_closing_brackets )
      ).
        DATA(lv_end_index) = lv_i - strlen( c_closing_brackets ). " -2 because of the }}.
        DATA(lv_length) = lv_end_index - lv_start_index.
        DATA(lv_offset) = lv_start_index - strlen( c_opening_brackets ).

        APPEND VALUE #(
          offset     = lv_offset
          length     = lv_i - lv_offset
          content    = iv_template_string+lv_start_index(lv_length)
          is_comment = xsdbool( lv_comment_type <> e_comment_type_none )
        ) TO rt_placeholders.

        " Reset values.
        lv_start_index = 0.
        lv_four_chars = ''.
        lv_previous_c = ''.
        lv_comment_type = e_comment_type_none.
      ELSE.
        lv_previous_c = lv_c.
      ENDIF.
    ENDWHILE.
  ENDMETHOD.


  METHOD tokenizer_add_token.
    APPEND VALUE #( value = iv_value position = iv_position type = iv_type ) TO me->mt_tokenizer_tokens.
  ENDMETHOD.


  METHOD parser_parse.
    me->mv_parser_index = 1.

    DATA(ls_result) = me->parser_eval_template( ).
    DATA(lv_error) = ls_result-error.

    IF lv_error IS NOT INITIAL.
      rv_error = lv_error.
      RETURN.
    ENDIF.

    me->mr_template = ls_result-stmt.
  ENDMETHOD.


  METHOD parser_build_error.
    rv_error = |{ iv_error } at position { is_token-position }|.
  ENDMETHOD.


  METHOD parser_build_expected_error.
    rv_error = me->parser_build_error( iv_error = |Expected { iv_error }| is_token = is_token ).
  ENDMETHOD.


  METHOD parser_peek_at.
    DATA(lv_index) = me->mv_parser_index + iv_at.

    CLEAR rs_token.

    IF lv_index <= lines( me->mt_tokenizer_tokens ).
      rs_token = me->mt_tokenizer_tokens[ lv_index ].
    ENDIF.
  ENDMETHOD.


  METHOD parser_peek.
    rs_token = me->parser_peek_at( iv_at = 0 ).
  ENDMETHOD.


  METHOD parser_eat.
    DATA(lv_index) = me->mv_parser_index.

    rs_token = me->parser_peek( ).

    IF lv_index < lines( me->mt_tokenizer_tokens ).
      me->mv_parser_index = lv_index + 1.
    ENDIF.
  ENDMETHOD.


  METHOD parser_eval_stmt.
    DATA ls_result TYPE ts_parser_eval_result.

    DATA(ls_token) = me->parser_peek( ).
    DATA(ls_next_token) = me->parser_peek_at( 1 ).
    DATA(lv_valid) = abap_true.
    DATA(lv_expect_eop) = abap_true.

    CASE ls_token-type.

        " If the current token is a # it's the beginning of a block.
      WHEN e_token_type_hashtag.
        CASE ls_next_token-type.
          WHEN e_token_type_path.
            ls_result = me->parser_eval_block( ).

          WHEN OTHERS.
            lv_valid = abap_false.
        ENDCASE.

        " If the current token is a text-token, its value is used directly.
      WHEN e_token_type_text.
        me->parser_eat( ).
        ls_result-stmt = NEW ts_parser_text( value = ls_token-value token = ls_token ).
        lv_expect_eop = abap_false.

        " If it's not a statement, it's probably an expression.
      WHEN OTHERS.
        ls_result = me->parser_eval_expr( ).
    ENDCASE.

    DATA(lv_error) = rs_result-error.

    IF lv_error IS NOT INITIAL.
      rs_result-error = lv_error.
      RETURN.
    ENDIF.

    IF lv_expect_eop <> abap_false.

      " Expect end-of-placeholder token.
      lv_error = me->parser_check_eop( ).

      IF lv_error IS NOT INITIAL.
        rs_result-error = lv_error.
        RETURN.
      ENDIF.
    ENDIF.

    IF lv_valid <> abap_true.
      rs_result-error = me->parser_build_error( iv_error = 'Unknown token' is_token = ls_token ).
      RETURN.
    ENDIF.

    rs_result = ls_result.
  ENDMETHOD.


  METHOD parser_eval_stmts.
    DATA lt_statements TYPE tt_parser_statements.
    DATA(lt_termination_tokens) = it_termination_token_types.

    " Make sure, EOF also terminates execution.
    APPEND e_token_type_eof TO lt_termination_tokens.

    DO.
      DATA(ls_token) = me->parser_peek( ).

      " Check if the current token is a termination token.
      IF xsdbool( VALUE #( lt_termination_tokens[ table_line = ls_token-type ] OPTIONAL ) IS NOT INITIAL ) = abap_true.
        EXIT.
      ENDIF.

      " Evaluate statement.
      DATA(ls_result) = me->parser_eval_stmt( ).
      DATA(lv_error) = ls_result-error.

      IF lv_error IS NOT INITIAL.
        rs_results-error = lv_error.
        RETURN.
      ENDIF.

      APPEND ls_result-stmt TO lt_statements.
    ENDDO.

    rs_results-stmts = lt_statements.
  ENDMETHOD.


  METHOD parser_eval_template.
    DATA(ls_template) = NEW ts_parser_template( ).
    DATA(ls_result) = me->parser_eval_stmts( ).
    DATA(lv_error) = ls_result-error.

    ls_template->body = VALUE #( statements = ls_result-stmts ).

    IF lv_error IS NOT INITIAL.
      rs_result-error = lv_error.
      RETURN.
    ENDIF.

    rs_result-stmt = ls_template.
  ENDMETHOD.


  METHOD parser_eval_block.
    DATA(ls_token) = me->parser_eat( ).
    DATA(ls_start_token) = ls_token.

    " Make sure block starts with #.
    IF ls_token-type <> e_token_type_hashtag.
      rs_result-error = me->parser_build_expected_error( iv_error = '#' is_token = ls_token ).
      RETURN.
    ENDIF.

    " Make sure a helper-name exists.
    DATA(ls_eval_helper_result) = me->parser_eval_helper_name( ).
    DATA(lv_error) = ls_eval_helper_result-error.

    IF lv_error IS NOT INITIAL.
      rs_result-error = lv_error.
      RETURN.
    ENDIF.

    DATA(lv_start_helper_name) = ls_eval_helper_result-name.
    DATA(ls_eval_args_result) = me->parser_eval_args( VALUE #( ( e_token_type_as ) ) ).

    lv_error = ls_eval_args_result-error.

    IF lv_error IS NOT INITIAL.
      rs_result-error = lv_error.
      RETURN.
    ENDIF.

    DATA(lt_args) = ls_eval_args_result-expressions.
    ls_token = me->parser_peek( ).

    " If next token is "as", block parameters are provided.
    IF ls_token-type = e_token_type_as.
      CONSTANTS c_pipe TYPE string VALUE '|'.

      me->parser_eat( ).
      ls_token = me->parser_eat( ).

      " Check if parameters get introduced via pipe.
      IF ls_token-type <> e_token_type_pipe.
        rs_result-error = me->parser_build_expected_error( iv_error = c_pipe is_token = ls_token ).
        RETURN.
      ENDIF.

      DATA lt_params TYPE tt_parser_block_params.

      DO.
        ls_token = me->parser_peek( ).

        " Cancel loop as soon as something else than a path expression gets discovered.
        IF ls_token-type <> e_token_type_path.
          EXIT.
        ENDIF.

        DATA(ls_eval_path_result) = me->parser_eval_path( ).
        lv_error = ls_eval_path_result-error.

        IF lv_error IS NOT INITIAL.
          rs_result-error = lv_error.
          RETURN.
        ENDIF.

        DATA lr_path TYPE REF TO ts_parser_path.
        lr_path ?= ls_eval_path_result-stmt.

        " A block parameter must only consist of one word.
        IF lr_path->is_identifier <> abap_true.
          rs_result-error = me->parser_build_expected_error( iv_error = 'identifier' is_token = ls_token ).
          RETURN.
        ENDIF.

        APPEND VALUE #( name = lr_path->parts[ 1 ] token = ls_token ) TO lt_params.
      ENDDO.

      ls_token = me->parser_eat( ).

      " Check if parameters get terminated via pipe.
      IF ls_token-type <> e_token_type_pipe.
        rs_result-error = me->parser_build_expected_error( iv_error = c_pipe is_token = ls_token ).
        RETURN.
      ENDIF.
    ENDIF.

    " Expect end-of-placeholder token.
    lv_error = me->parser_check_eop( ).

    IF lv_error IS NOT INITIAL.
      rs_result-error = lv_error.
      RETURN.
    ENDIF.

    ls_token = me->parser_peek( ).

    " Evaluate block-statements.
    DATA(ls_stmts_result) = me->parser_eval_stmts(
      it_termination_token_types = VALUE #( ( e_token_type_else ) ( e_token_type_slash ) ) " Terminate on else and /.
    ).
    lv_error = ls_stmts_result-error.

    IF lv_error IS NOT INITIAL.
      rs_result-error = lv_error.
      RETURN.
    ENDIF.

    " Create instance of ts_parser_block on the heap.
    DATA(ls_block) = NEW ts_parser_block(
      name   = lv_start_helper_name
      args   = lt_args
      body   = VALUE ts_parser_body( statements = ls_stmts_result-stmts token = ls_token )
      params = lt_params
      token  = ls_start_token
    ).

    ls_token = me->parser_peek( ).

    " If the statements were terminated by an else, parse the rest.
    IF ls_token-type = e_token_type_else.
      me->parser_eat( ).

      " Expect end-of-placeholder token.
      lv_error = me->parser_check_eop( ).

      IF lv_error IS NOT INITIAL.
        rs_result-error = lv_error.
        RETURN.
      ENDIF.

      " Evaluate else-statements.
      ls_stmts_result = me->parser_eval_stmts(
        it_termination_token_types = VALUE #( ( e_token_type_slash ) ) " Terminate on /.
      ).
      lv_error = ls_stmts_result-error.

      IF lv_error IS NOT INITIAL.
        rs_result-error = lv_error.
        RETURN.
      ENDIF.

      ls_block->else = VALUE ts_parser_body( statements = ls_stmts_result-stmts ).
    ENDIF.

    ls_token = me->parser_eat( ).

    " Make sure block is terminated with a slash...
    IF ls_token-type <> e_token_type_slash.
      rs_result-error = me->parser_build_expected_error( iv_error = '/' is_token = ls_token ).
      RETURN.
    ENDIF.

    ls_token = me->parser_eat( ).

    DATA(lv_end_helper_name) = ls_token-value.

    " ...followed by the helper name.
    IF ls_token-type <> e_token_type_path OR lv_end_helper_name <> lv_start_helper_name.
      rs_result-error = me->parser_build_expected_error( iv_error = |{ lv_start_helper_name } but got { lv_end_helper_name }| is_token = ls_token ).
      RETURN.
    ENDIF.

    rs_result-stmt = ls_block.
  ENDMETHOD.


  METHOD parser_eval_inline_helper.
    DATA(ls_token) = me->parser_peek( ).
    DATA(ls_eval_helper_result) = me->parser_eval_helper_name( ).
    DATA(lv_error) = ls_eval_helper_result-error.

    IF lv_error IS NOT INITIAL.
      rs_result-error = lv_error.
      RETURN.
    ENDIF.

    " Evaluate helper arguments.
    DATA(ls_eval_args_result) = me->parser_eval_args( it_termination_token_types ).
    lv_error = ls_eval_args_result-error.

    IF lv_error IS NOT INITIAL.
      rs_result-error = lv_error.
      RETURN.
    ENDIF.

    rs_result-stmt = NEW ts_parser_inline_helper(
      name  = ls_eval_helper_result-name
      args  = ls_eval_args_result-expressions
      token = ls_token
    ).
  ENDMETHOD.


  METHOD parser_eval_expr.
    DATA lr_data TYPE REF TO data.

    DATA(ls_token) = me->parser_peek( ).
    DATA(lv_token_type) = ls_token-type.
    DATA(lv_value) = ls_token-value.
    DATA(lv_eat) = abap_true.

    CASE lv_token_type.
      WHEN e_token_type_bool_literal.
        lr_data = NEW ts_parser_bool_literal( value = xsdbool( lv_value <> c_false ) token = ls_token ).

      WHEN e_token_type_number_literal.
        lr_data = NEW ts_parser_float_literal( value = CONV i( lv_value ) token = ls_token ).

      WHEN e_token_type_string_literal.
        lr_data = NEW ts_parser_string_literal( value = lv_value token = ls_token ).

      WHEN e_token_type_path OR e_token_type_o_round_bracket.
        DATA ls_result TYPE ts_parser_eval_result.

        CASE lv_token_type.
          WHEN e_token_type_o_round_bracket.
            ls_result = me->parser_eval_sub_expr( ).

          WHEN OTHERS.
            DATA(lt_term_token_types) = it_termination_token_types.

            " Make sure EOP is part of termination token list.
            APPEND e_token_type_eop TO lt_term_token_types.

            DATA(ls_next_token) = me->parser_peek_at( 1 ).
            DATA(lv_next_token_type) = ls_next_token-type.
            DATA(lv_is_term_token) = VALUE #( lt_term_token_types[ table_line = lv_next_token_type ] OPTIONAL ).

            " If the next token is not in termination token list, it's an inline-helper.
            IF ls_next_token-type <> lv_is_term_token.
              ls_result = me->parser_eval_inline_helper( lt_term_token_types ).
            ELSE.
              ls_result = me->parser_eval_path( ).
            ENDIF.
        ENDCASE.

        DATA(lv_error) = ls_result-error.

        IF lv_error IS NOT INITIAL.
          rs_result-error = lv_error.
          RETURN.
        ENDIF.

        lr_data = ls_result-stmt.
        lv_eat = abap_false.

      WHEN e_token_type_null.
        lr_data = NEW ts_parser_null_literal( token = ls_token ).

      WHEN e_token_type_undefined.
        lr_data = NEW ts_parser_undefined_literal( token = ls_token ).

      WHEN OTHERS.
        rs_result-error = me->parser_build_error( iv_error = |Unknown expression type| is_token = ls_token ).
    ENDCASE.

    IF lv_eat <> abap_false.
      me->parser_eat( ).
    ENDIF.

    rs_result-stmt = lr_data.
  ENDMETHOD.


  METHOD parser_eval_sub_expr.
    DATA(ls_token) = me->parser_eat( ).
    DATA(ls_bracket_token) = ls_token.

    IF ls_token-type <> e_token_type_o_round_bracket.
      rs_result-error = me->parser_build_expected_error( iv_error = '(' is_token = ls_token ).
      RETURN.
    ENDIF.

    DATA(ls_result) = parser_eval_expr( VALUE #( ( e_token_type_c_round_bracket ) ) ).
    DATA(lv_error) = ls_result-error.

    IF lv_error IS NOT INITIAL.
      rs_result-error = lv_error.
      RETURN.
    ENDIF.

    ls_token = me->parser_eat( ).

    IF ls_token-type <> e_token_type_c_round_bracket.
      rs_result-error = me->parser_build_expected_error( iv_error = ')' is_token = ls_token ).
      RETURN.
    ENDIF.

    rs_result-stmt = NEW ts_parser_sub_expr(
      expr  = ls_result-stmt
      token = ls_bracket_token
    ).
  ENDMETHOD.


  METHOD parser_eval_path.
    DATA lt_collected_parts TYPE string_table.

    DATA(ls_token) = me->parser_eat( ).

    IF ls_token-type <> e_token_type_path.
      rs_result-error = me->parser_build_expected_error( iv_error = 'path' is_token = ls_token ).
      RETURN.
    ENDIF.

    DATA(lv_value) = ls_token-value.

    " First split at slashes.
    SPLIT lv_value AT '/' INTO TABLE DATA(lt_relative_parts).

    LOOP AT lt_relative_parts INTO DATA(lv_relative_part).
      IF lv_relative_part = c_relative.
        APPEND lv_relative_part TO lt_collected_parts.
      ELSE.

        " Now split at single dots.
        SPLIT lv_relative_part AT '.' INTO TABLE DATA(lt_parts).

        LOOP AT lt_parts INTO DATA(lv_part).
          APPEND lv_part TO lt_collected_parts.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

    rs_result-stmt = NEW ts_parser_path(
      parts         = lt_collected_parts
      is_identifier = xsdbool( lines( lt_collected_parts ) = 1 AND lt_collected_parts[ 1 ] <> c_relative )
      token         = ls_token
    ).
  ENDMETHOD.


  METHOD parser_eval_helper_name.
    DATA(ls_token) = me->parser_peek( ).
    DATA(lv_name_error) = me->parser_build_expected_error( iv_error = 'helper name' is_token = ls_token ).

    IF ls_token-type <> e_token_type_path.
      rs_result-error = lv_name_error.
      RETURN.
    ENDIF.

    DATA(ls_result) = me->parser_eval_path( ).
    DATA(lv_error) = ls_result-error.

    IF lv_error IS NOT INITIAL.
      rs_result-error = lv_error.
      RETURN.
    ENDIF.

    DATA lr_path TYPE REF TO ts_parser_path.
    lr_path ?= ls_result-stmt.

    IF lr_path->is_identifier <> abap_true.
      rs_result-error = lv_name_error.
      RETURN.
    ENDIF.

    rs_result-name = ls_token-value.
  ENDMETHOD.


  METHOD parser_eval_args.
    DATA lt_args TYPE tt_parser_expressions.
    DATA(lt_termination_token_types) = it_termination_token_types.

    " Add safety net.
    APPEND e_token_type_eop TO lt_termination_token_types.
    APPEND e_token_type_eof TO lt_termination_token_types.

    DO.
      DATA(ls_token) = me->parser_peek( ).
      DATA(lv_found_type) = VALUE #( lt_termination_token_types[ table_line = ls_token-type ] OPTIONAL ).

      IF lv_found_type IS NOT INITIAL.
        EXIT.
      ENDIF.

      DATA(ls_expr_result) = me->parser_eval_expr( lt_termination_token_types ).
      DATA(lv_error) = ls_expr_result-error.

      IF lv_error IS NOT INITIAL.
        rs_result-error = lv_error.
        RETURN.
      ENDIF.

      APPEND ls_expr_result-stmt TO lt_args.
    ENDDO.

    rs_result-expressions = lt_args.
  ENDMETHOD.


  METHOD parser_check_eop.
    DATA ls_token TYPE ts_tokenizer_token.

    IF lv_peek <> abap_false.
      ls_token = me->parser_peek( ).
    ELSE.
      ls_token = me->parser_eat( ).
    ENDIF.

    IF ls_token-type <> e_token_type_eop.
      rv_error = me->parser_build_expected_error( iv_error = 'End of placeholder' is_token = ls_token ).
    ENDIF.
  ENDMETHOD.


  METHOD backend_build_error.
    rv_error = me->parser_build_error( iv_error = iv_error is_token = is_token ).
  ENDMETHOD.


  METHOD backend_n_args_helper_error.
    rv_error = me->error( |Expected exactly { iv_expected } argument but got { iv_actual }| ).
  ENDMETHOD.


  METHOD backend_eval_body.
    DATA lv_text TYPE string.

    LOOP AT ir_block-statements INTO DATA(lr_stmt).
      DATA(ls_result) = me->backend_eval_stmt(
        ir_stmt = lr_stmt
        is_data = is_data
      ).
      DATA(lv_error) = ls_result-error.

      IF lv_error IS NOT INITIAL.
        rs_result-error = lv_error.
        RETURN.
      ENDIF.

      lv_text = |{ lv_text }{ ls_result-text }|.
    ENDLOOP.

    rs_result-text = lv_text.
  ENDMETHOD.


  METHOD backend_eval_stmt.
    DATA(ls_type) = me->get_data_type( ir_stmt ).
    DATA(ls_type_name) = ls_type-name.

    CASE ls_type_name.
      WHEN 'ts_parser_template'.
        DATA lr_template TYPE REF TO ts_parser_template.
        lr_template ?= ir_stmt.

        rs_result = me->backend_eval_body(
          ir_block = lr_template->body
          is_data  = is_data
        ).

      WHEN 'ts_parser_text'.
        DATA lr_text TYPE REF TO ts_parser_text.
        lr_text ?= ir_stmt.

        rs_result-text = lr_text->value.

      WHEN 'ts_parser_block'.
        DATA lr_conditional_block TYPE REF TO ts_parser_block.
        lr_conditional_block ?= ir_stmt.

        rs_result = me->backend_eval_block(
          ir_block = lr_conditional_block
          is_data  = is_data
        ).

      WHEN OTHERS.
        DATA(ls_eval_expr_result) = me->backend_eval_expr(
          ir_stmt = ir_stmt
          is_data = is_data
        ).
        DATA(lv_error) = ls_eval_expr_result-error.
        DATA(lv_kind) = ls_eval_expr_result-kind.

        IF lv_error IS NOT INITIAL.
          rs_result-error = lv_error.
          RETURN.
        ELSEIF lv_kind <> e_backend_data_kind_simple.
          DATA(ls_token) = me->backend_get_token_property( ir_struct = ir_stmt ).
          rs_result-error = me->backend_build_error( iv_error = |Cannot convert { lv_kind } to text| is_token = ls_token ).
          RETURN.
        ENDIF.

        " Use string interpolation to convert to string as using
        " ls_eval_expr_result-data->* directly might result in
        " unwanted spaces (e.g. for positive number a space is
        " added at the end, while for negative numbers a "-" is
        " added at the end...
        rs_result-text = |{ ls_eval_expr_result-data->* }|.
    ENDCASE.
  ENDMETHOD.


  METHOD backend_eval_expr.

    " First, try to evaluate if it's a literal.
    DATA(ls_literal_result) = me->backend_eval_literal_expr( ir_stmt = ir_stmt is_data = is_data ).

    " If no error occurred, it's a literal which can be returned immediately.
    IF ls_literal_result-error IS INITIAL.
      rs_result = ls_literal_result.
      RETURN.
    ENDIF.

    " If it's not a literal, try to evaluate a more complex type.
    DATA: lr_data  TYPE REF TO data,
          lv_error TYPE string.

    DATA(lv_type) = me->get_data_type( ir_stmt ).
    DATA(lv_type_name) = lv_type-name.

    CASE lv_type_name.
      WHEN 'ts_parser_sub_expr'.
        DATA lr_sub_expr TYPE REF TO ts_parser_sub_expr.
        lr_sub_expr ?= ir_stmt.

        DATA(ls_sub_expr_result) = me->backend_eval_sub_expr(
          ir_sub_expr = lr_sub_expr
          is_data     = is_data
        ).
        lv_error = ls_sub_expr_result-error.

        IF lv_error IS NOT INITIAL.
          rs_result-error = lv_error.
          RETURN.
        ENDIF.

        lr_data = ls_sub_expr_result-data.

      WHEN 'ts_parser_inline_helper'.
        DATA lr_inline_helper TYPE REF TO ts_parser_inline_helper.
        lr_inline_helper ?= ir_stmt.

        DATA(ls_inline_helper_result) = me->backend_eval_inline_helper(
          ir_inline_helper = lr_inline_helper
          is_data          = is_data
        ).
        lv_error = ls_inline_helper_result-error.

        IF lv_error IS NOT INITIAL.
          rs_result-error = lv_error.
          RETURN.
        ENDIF.

        lr_data = NEW string( ls_inline_helper_result-text ).

      WHEN 'ts_parser_path'.
        DATA lr_path TYPE REF TO ts_parser_path.
        lr_path ?= ir_stmt.

        DATA(ls_path_result) = me->backend_eval_path(
          ir_path = lr_path
          is_data = is_data
        ).
        lv_error = ls_path_result-error.

        IF lv_error IS NOT INITIAL.
          rs_result-error = lv_error.
          RETURN.
        ENDIF.

        lr_data = ls_path_result-data.

      WHEN OTHERS.
        DATA(ls_token) = me->backend_get_token_property( ir_stmt ).

        rs_result-error = me->backend_build_error( iv_error = |Unknown expression type { lv_type_name }| is_token = ls_token ).
        RETURN.
    ENDCASE.

    rs_result-data = lr_data.
    rs_result-kind = me->backend_get_data_kind( lr_data ).
  ENDMETHOD.


  METHOD backend_eval_literal_expr.
    DATA: ls_data TYPE REF TO data,
          lr_data TYPE REF TO data.

    DATA(lv_type) = me->get_data_type( ir_stmt ).
    DATA(lv_type_name) = lv_type-name.

    CASE lv_type_name.
      WHEN 'ts_parser_bool_literal'.
        lr_data = ir_stmt.

      WHEN 'ts_parser_float_literal'.
        lr_data = ir_stmt.

      WHEN 'ts_parser_string_literal'.
        lr_data = ir_stmt.

      WHEN 'ts_parser_null_literal' OR 'ts_parser_undefined'.
        lr_data = ir_stmt.

      WHEN OTHERS.
        DATA(ls_token) = me->backend_get_token_property( ir_stmt ).

        rs_result-error = me->backend_build_error( iv_error = |Unknown literal type { lv_type_name }| is_token = ls_token ).
        RETURN.
    ENDCASE.

    rs_result-data = lr_data.
    rs_result-kind = me->backend_get_data_kind( lr_data ).
  ENDMETHOD.


  METHOD backend_eval_helper.
    DATA: lt_args  TYPE tt_data,
          lv_error TYPE string.

    " "Downcast" to common base.
    DATA(lr_helper) = NEW ts_parser_helper( ).
    MOVE-CORRESPONDING ir_helper->* TO lr_helper->*.

    IF lr_helper->name IS INITIAL.
      rs_result-error = 'Invalid helper cast'.
      RETURN.
    ENDIF.

    " Evaluate arguments.
    LOOP AT lr_helper->args INTO DATA(ls_arg).
      DATA(ls_result) = me->backend_eval_expr(
        ir_stmt = ls_arg
        is_data = is_data
      ).
      lv_error = ls_result-error.

      IF lv_error IS NOT INITIAL.
        rs_result-error = lv_error.
        RETURN.
      ENDIF.

      APPEND ls_result-data TO lt_args.
    ENDLOOP.

    " Find out if it's a block- or an inline-helper.
    DATA lr_block TYPE REF TO ts_parser_block.

    TRY.
        lr_block ?= ir_helper.
      CATCH cx_root.
        " Nothing to do.
    ENDTRY.

    DATA(lv_is_block) = xsdbool( lr_block IS BOUND ).

    " Push current block to stack for context information...
    IF lv_is_block = abap_true.
      APPEND VALUE #( block = lr_block ) TO me->mt_backend_block_stack.
    ELSE.
      " ...or set current inline helper values.
      MOVE-CORRESPONDING ir_helper->* TO me->mv_backend_inline_helper.
    ENDIF.

    rs_result = me->backend_call_helper(
      iv_name = lr_helper->name
      it_args = lt_args
      is_data = is_data
    ).

    " Pop last entry from block stack.
    IF lv_is_block = abap_true.
      DELETE me->mt_backend_block_stack INDEX lines( me->mt_backend_block_stack ).
    ELSE.
      CLEAR me->mv_backend_inline_helper.
    ENDIF.
  ENDMETHOD.


  METHOD backend_eval_block.
    rs_result = me->backend_eval_helper(
      ir_helper = ir_block
      is_data   = is_data
    ).
  ENDMETHOD.


  METHOD backend_eval_block_helper.

    " Only allow fn-/reverse-invocation if current helper is not an inline-element.
    IF me->mv_backend_inline_helper IS INITIAL.
      DATA(lr_block) = me->backend_get_last_block( ).

      " Return nothing. This method is not meant to be called externally.
      IF lr_block IS NOT BOUND.
        RETURN.
      ENDIF.

      DATA(lr_parser_block) = lr_block->block.

      DATA ls_body TYPE ts_parser_body.
      ASSIGN COMPONENT iv_property OF STRUCTURE lr_parser_block->* TO FIELD-SYMBOL(<body>).
      ls_body = <body>.

      CLEAR lr_block->args.
      DATA(lv_index) = 1.

      " Convert ia_data to tt_data, if required.
      DATA lt_data TYPE tt_data.
      DATA(lr_data) = me->any_to_ref_to_data( ia_data ).
      DATA(ls_type) = me->get_data_type( lr_data ).
      DATA(lv_is_ref) = ls_type-is_ref.
      DATA(lv_kind) = me->backend_get_data_kind( lr_data ).

      IF lv_kind <> e_backend_data_kind_table.
        lt_data = VALUE #( ( lr_data ) ).
      ELSE.
        lt_data = lr_data->*.
      ENDIF.

      " Fill block parameters with values.
      LOOP AT lt_data INTO DATA(ls_data).
        READ TABLE lr_parser_block->params INTO DATA(ls_parser_block_param) INDEX lv_index.

        " If no parameter could be read for the current index, no more
        " parameters have been provided.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.

        APPEND VALUE #( param = ls_parser_block_param data = ls_data ) TO lr_block->args.
        lv_index = lv_index + 1.
      ENDLOOP.

      ls_data = VALUE #( lt_data[ 1 ] OPTIONAL ).

      rs_result = me->backend_eval_body(
        ir_block = ls_body
        is_data  = ls_data
      ).
    ENDIF.
  ENDMETHOD.


  METHOD backend_eval_cond_helper.

    DATA(lv_lines) = lines( it_args ).

    IF lv_lines <> 1.
      rs_result-error = me->backend_n_args_helper_error( iv_expected = 1 iv_actual = lv_lines ).
      RETURN.
    ENDIF.

    DATA(lr_condition) = it_args[ 1 ].
    DATA(ls_truthy_result) = me->is_truthy( lr_condition ).
    DATA(lv_error) = ls_truthy_result-error.

    IF lv_error IS NOT INITIAL.
      rs_result-error = lv_error.
      RETURN.
    ENDIF.

    DATA(lv_condition_is_true) = xsdbool( ls_truthy_result-truthy = abap_true ).

    " If unless, reverse the condition result.
    IF iv_name = c_unless.
      lv_condition_is_true = xsdbool( lv_condition_is_true = abap_false ).
    ENDIF.

    IF lv_condition_is_true = abap_true.
      rs_result = me->fn( is_data ).
    ELSE.
      rs_result = me->inverse( is_data ).
    ENDIF.
  ENDMETHOD.


  METHOD backend_eval_each_helper.
    DATA(lv_lines) = lines( it_args ).

    IF lv_lines <> 1.
      rs_result-error = me->backend_n_args_helper_error( iv_expected = 1 iv_actual = lv_lines ).
      RETURN.
    ENDIF.

    DATA: ls_result TYPE ts_text_result,
          lv_error  TYPE string.

    DATA(lr_iterable) = it_args[ 1 ].
    DATA(lv_type) = me->backend_get_data_kind( lr_iterable ).
    DATA(ls_truthy_result) = me->is_truthy( lr_iterable ).

    lv_error = ls_truthy_result-error.

    IF lv_error IS NOT INITIAL.
      rs_result-error = lv_error.
      RETURN.
    ENDIF.

    IF ls_truthy_result-truthy = abap_true.
      DATA ls_data TYPE REF TO data.
      DATA(lv_text) = VALUE string( ).

      CASE lv_type.
        WHEN e_backend_data_kind_struct.
          DATA(lo_struct_desc) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data_ref( lr_iterable ) ).
          DATA(lt_components) = lo_struct_desc->get_components( ).

          IF lines( lt_components ) > 0.
            LOOP AT lt_components INTO DATA(ls_field).
              DATA(lv_field_name) = ls_field-name.

              ASSIGN COMPONENT lv_field_name OF STRUCTURE lr_iterable->* TO FIELD-SYMBOL(<field>).

              GET REFERENCE OF <field> INTO DATA(lr_field).
              GET REFERENCE OF lv_field_name INTO DATA(lr_key).

              ls_result = me->fn( VALUE tt_data( ( lr_field ) ( lr_key ) ) ).
              lv_error = ls_result-error.

              IF lv_error IS NOT INITIAL.
                rs_result-error = lv_error.
                RETURN.
              ENDIF.

              lv_text = |{ lv_text }{ ls_result-text }|.
            ENDLOOP.
          ENDIF.

        WHEN e_backend_data_kind_table.
          FIELD-SYMBOLS: <table> TYPE ANY TABLE.

          ASSIGN lr_iterable->* TO <table>.

          DATA(lv_index) = 0. " 0 to stay consistent with Handlebars' implementation.

          LOOP AT <table> ASSIGNING FIELD-SYMBOL(<row>).
            DATA lr_row TYPE REF TO data.
            DATA(lo_row_desc) = cl_abap_typedescr=>describe_by_data( <row> ).

            " If row is bound, it's a reference, otherwise it's a value.
            IF lo_row_desc->kind = lo_row_desc->kind_ref.
              lr_row = <row>.
            ELSE.
              GET REFERENCE OF <row> INTO lr_row.
            ENDIF.

            GET REFERENCE OF lv_index INTO DATA(lr_index).

            ls_result = me->fn( VALUE tt_data( ( lr_row ) ( lr_index ) ) ).
            lv_error = ls_result-error.

            IF lv_error IS NOT INITIAL.
              rs_result-error = lv_error.
              RETURN.
            ENDIF.

            lv_text = |{ lv_text }{ ls_result-text }|.
            lv_index = lv_index + 1.
          ENDLOOP.

        WHEN OTHERS.
          rs_result-error = me->error( 'Data is neither a structure nor an object' ).
          RETURN.
      ENDCASE.

      rs_result-text = lv_text.
    ELSE.
      rs_result = me->inverse( is_data ).
    ENDIF.
  ENDMETHOD.


  METHOD backend_eval_with_helper.
    DATA(lv_lines) = lines( it_args ).

    IF lv_lines <> 1.
      rs_result-error = me->backend_n_args_helper_error( iv_expected = 1 iv_actual = lv_lines ).
      RETURN.
    ENDIF.

    DATA(lr_data) = it_args[ 1 ].
    DATA(ls_truthy_result) = me->is_truthy( lr_data ).
    DATA(lv_error) = ls_truthy_result-error.

    IF lv_error IS NOT INITIAL.
      rs_result-error = lv_error.
      RETURN.
    ENDIF.

    IF ls_truthy_result-truthy = abap_true.
      rs_result = me->fn( lr_data ).
    ELSE.
      rs_result = me->inverse( lr_data ).
    ENDIF.
  ENDMETHOD.


  METHOD backend_eval_inline_helper.
    rs_result = me->backend_eval_helper(
      ir_helper = ir_inline_helper
      is_data   = is_data
    ).
  ENDMETHOD.


  METHOD backend_eval_log_helper.
    DATA lv_log_text TYPE string.

    LOOP AT it_args INTO DATA(ls_arg).
      IF lv_log_text <> ' '.
        lv_log_text = |{ lv_log_text } |.
      ENDIF.

      lv_log_text = |{ lv_log_text }{ ls_arg->* }|.
    ENDLOOP.

    IF lv_log_text <> ' '.
      WRITE / lv_log_text.
    ENDIF.
  ENDMETHOD.


  METHOD backend_eval_sub_expr.
    rs_result = me->backend_eval_expr(
      ir_stmt = ir_sub_expr->expr
      is_data = is_data
    ).
  ENDMETHOD.


  METHOD backend_eval_path.
    DATA: lr_this        TYPE REF TO data,
          lr_block       TYPE REF TO ts_backend_block_stack_block,
          lv_block_index TYPE i.

    DATA(ls_token) = ir_path->token.
    DATA(lt_parts) = ir_path->parts.
    DATA(lv_relative_path_found) = abap_false.
    DATA(lv_undefined) = abap_false.
    DATA(lv_index) = 1.
    DATA(lv_original_block_index) = lines( me->mt_backend_block_stack ).

    " Go back the amount of relative steps.
    WHILE lv_index <= lines( lt_parts ).
      IF lt_parts[ 1 ] = c_relative.
        lv_block_index = lv_original_block_index - lv_index.
        lr_block = me->backend_get_block( lv_block_index ).

        DELETE lt_parts INDEX 1.

        " At worst, resolve to the root.
        IF lr_block IS NOT BOUND.
          lr_this = me->mr_root_data.
          EXIT.
        ENDIF.

        DATA(lt_block_args) = lr_block->args.

        IF lines( lt_block_args ) > 0.
          lr_this = lt_block_args[ 1 ]-data.
        ENDIF.

        lv_index = lv_index + 1.
      ELSE.
        EXIT.
      ENDIF.
    ENDWHILE.

    " If no data has been found, reset block index.
    IF lr_this IS NOT BOUND.
      lv_block_index = lv_original_block_index.
      lr_this = is_data.
    ELSE.
      lv_original_block_index = lv_block_index.
    ENDIF.

    DATA(lv_lines) = lines( lt_parts ).
    lv_index = 1.

    DATA lv_kind TYPE e_backend_data_kinds.
    DATA(lv_only_down) = abap_false.
    DATA(lv_property_found) = abap_false.

    WHILE lv_undefined = abap_false AND lv_index <= lv_lines.
      DATA(lv_part) = lt_parts[ lv_index ].
      DATA(lv_skip) = abap_false.

      " Only evaluate "this" or block-parameter for first index.
      IF lv_index = 1.

        " Check for "this"-keyword.
        IF lv_part = c_this.
          lv_skip = abap_true.
          lv_only_down = abap_true.

          " If no relative path was found, check for block parameter.
        ELSEIF lv_relative_path_found = abap_false.

          " Use a do-loop to look for a properly named block parameter in the block stack from bottom to top (latest first).
          DO.
            lr_block = backend_get_block( lv_block_index ).

            IF lr_block IS NOT BOUND.
              EXIT.
            ENDIF.

            DATA(ls_arg) = VALUE #( lr_block->args[ param-name = lv_part ] OPTIONAL ).

            IF ls_arg IS NOT INITIAL.
              lr_this ?= ls_arg-data.

              " Try to evaluate if found data is a literal.
              DATA(ls_literal_result) = me->backend_eval_literal_expr( ir_stmt = lr_this ).

              lv_only_down = abap_true.

              " If it is a literal, evaluate its value.
              IF ls_literal_result-error IS INITIAL.
                ASSIGN COMPONENT 'value' OF STRUCTURE lr_this->* TO FIELD-SYMBOL(<value>).

                IF sy-subrc <> 0.
                  lv_undefined = abap_true.
                  EXIT.
                ENDIF.

                GET REFERENCE OF <value> INTO lr_this.
              ENDIF.

              lv_skip = abap_true.
              EXIT.
            ENDIF.

            lv_block_index = lv_block_index - 1.
          ENDDO.
        ENDIF.
      ENDIF.

      " If undefined has been discovered, exit loop.
      IF lv_undefined <> abap_false.
        EXIT.
      ENDIF.

      " Use a do-loop to be able to go upwards in the structure tree to look for a property.
      DO.

        " Skip if the first path part was either "this" or a block-parameter name.
        IF lv_skip = abap_false.
          lv_kind = me->backend_get_data_kind( lr_this ).

          " Check if there's something to check the property on.
          IF lv_kind = e_backend_data_kind_undefined.
            lv_undefined = abap_true.
          ELSE.
            lv_property_found = abap_false.

            " Check if data is a structure.
            IF lv_kind = e_backend_data_kind_struct.
              ASSIGN COMPONENT lv_part OF STRUCTURE lr_this->* TO FIELD-SYMBOL(<field>).

              " Check if requested path exists.
              IF sy-subrc = 0.
                lv_property_found = abap_true.

                GET REFERENCE OF <field> INTO lr_this.
                EXIT.
              ENDIF.
            ENDIF.

            " If no property has been found yet, go further up.
            IF lv_property_found = abap_false.

              " Only go further up if it's allowed.
              IF lv_only_down = abap_false.
                lr_block = me->backend_get_block( lv_block_index ).

                IF lr_block IS NOT BOUND.
                  lv_undefined = abap_true.
                  EXIT.
                ENDIF.

                lt_block_args = lr_block->args.

                IF lines( lt_block_args ) > 0.
                  lr_this = lt_block_args[ 1 ]-data.
                ELSE.
                  lv_undefined = abap_true.
                  EXIT.
                ENDIF.

                lv_block_index = lv_block_index - 1.
              ELSE.
                EXIT.
              ENDIF.
            ENDIF.
          ENDIF.
        ELSE.
          EXIT.
        ENDIF.
      ENDDO.

      lv_index = lv_index + 1.
    ENDWHILE.

    " If lr_data is not bound, it means that there's no structure to look up for the property at.
    IF lr_this IS NOT BOUND AND lv_property_found = abap_false.

      " If it's the first iteration and no property has been found, it's possible
      " that the path refers to an inline-helper.
      IF lv_index = 1.
        DATA(ls_find_helper_result) = me->find_helper( ir_instance = me iv_name = lv_part ).

        " If it's a helper, invoke it and use the result.
        IF ls_find_helper_result-error IS INITIAL.
          DATA(rs_helper_result) = me->backend_call_helper(
            iv_name = lv_part
            is_data = is_data
          ).
          DATA(lv_error) = rs_helper_result-error.

          IF lv_error IS NOT INITIAL.
            rs_helper_result-error = lv_error.
            RETURN.
          ENDIF.

          lr_this = NEW string( rs_helper_result-text ).
          EXIT.
        ENDIF.
      ENDIF.

      lv_undefined = abap_true.
      EXIT.
    ENDIF.

    " If the last found property is undefined, create an empty string.
    IF lv_undefined = abap_true OR lv_kind = e_backend_data_kind_undefined.
      lr_this = NEW string( ).
    ENDIF.

    rs_result-data = lr_this.
    rs_result-kind = me->backend_get_data_kind( lr_this ).
  ENDMETHOD.


  METHOD backend_get_data_kind.
    rv_kind = e_backend_data_kind_undefined.

    IF ir_data IS BOUND.
      DATA(ls_descriptor) = cl_abap_typedescr=>describe_by_data_ref( ir_data ).

      CASE ls_descriptor->kind.
        WHEN ls_descriptor->kind_elem.
          rv_kind = e_backend_data_kind_simple.

        WHEN ls_descriptor->kind_struct.
          rv_kind = e_backend_data_kind_struct.

        WHEN ls_descriptor->kind_table.
          rv_kind = e_backend_data_kind_table.

        WHEN OTHERS.
          rv_kind = e_backend_data_kind_unknown.
      ENDCASE.
    ENDIF.
  ENDMETHOD.


  METHOD backend_get_block.
    READ TABLE me->mt_backend_block_stack REFERENCE INTO rr_block INDEX iv_index.

    IF sy-subrc <> 0.
      FREE rr_block.
    ENDIF.
  ENDMETHOD.


  METHOD backend_get_last_block.
    rr_block = me->backend_get_block( lines( mt_backend_block_stack ) ).
  ENDMETHOD.


  METHOD is_truthy.
    DATA(lv_kind) = me->backend_get_data_kind( ir_data ).
    DATA(lv_truthy) = abap_false.

    CASE lv_kind.
      WHEN e_backend_data_kind_simple.
        lv_truthy = xsdbool( ir_data->* <> ' ' ).

      WHEN e_backend_data_kind_struct.
        DATA(lo_struct_desc) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data_ref( ir_data ) ).
        DATA(lt_components) = lo_struct_desc->get_components( ).

        lv_truthy = xsdbool( lines( lt_components ) > 0 ).

      WHEN e_backend_data_kind_table.
        FIELD-SYMBOLS: <table> TYPE ANY TABLE.

        ASSIGN ir_data->* TO <table>.
        lv_truthy = xsdbool( lines( <table> ) > 0 ).

      WHEN OTHERS.
        rs_result-error = |Unknown data kind { lv_kind }|.

    ENDCASE.

    rs_result-truthy = lv_truthy.
  ENDMETHOD.


  METHOD backend_call_helper.
    DATA(ls_find_helper_result) = me->find_helper( ir_instance = me iv_name = iv_name ).
    DATA(lv_error) = ls_find_helper_result-error.

    IF lv_error IS NOT INITIAL.
      rs_result-error = lv_error.
      RETURN.
    ENDIF.

    DATA(lr_registered_helper) = ls_find_helper_result-helper.
    DATA(lr_helper) = lr_registered_helper->helper.
    DATA(ls_type) = me->get_data_type( lr_helper ).

    TRY.
        DATA(lv_type_name) = ls_type-name.

        CASE lv_type_name.
          WHEN 'ts_class_helper'.
            DATA ls_class_helper_config TYPE REF TO ts_class_helper.
            ls_class_helper_config ?= lr_helper.

            DATA(lv_class_name) = ls_class_helper_config->class_name.
            DATA(lv_class_method_name) = ls_class_helper_config->method_name.

            TRANSLATE lv_class_name TO UPPER CASE.
            TRANSLATE lv_class_method_name TO UPPER CASE.

            CALL METHOD (lv_class_name)=>(lv_class_method_name)
              EXPORTING
                io_instance = me
                iv_name     = iv_name
                it_args     = it_args
                is_data     = is_data
              RECEIVING
                rs_result   = rs_result.

          WHEN 'ts_object_helper'.
            DATA ls_object_helper_config TYPE REF TO ts_object_helper.
            ls_object_helper_config ?= lr_helper.

            DATA(lv_object_method_name) = ls_object_helper_config->method_name.
            TRANSLATE lv_object_method_name TO UPPER CASE.

            CALL METHOD ls_object_helper_config->object->(lv_object_method_name)
              EXPORTING
                io_instance = me
                iv_name     = iv_name
                it_args     = it_args
                is_data     = is_data
              RECEIVING
                rs_result   = rs_result.

          WHEN 'ts_func_module_helper'.
            DATA ls_func_module_helper_config TYPE REF TO ts_func_module_helper.
            ls_func_module_helper_config ?= lr_helper.

            DATA(lv_function_name) = ls_func_module_helper_config->function_name.
            TRANSLATE lv_function_name TO UPPER CASE.

            CALL FUNCTION ls_func_module_helper_config->function_name
              EXPORTING
                io_instance = me
                iv_name     = iv_name
                it_args     = it_args
                is_data     = is_data
              IMPORTING
                es_result   = rs_result.

          WHEN 'ts_form_helper'.
            DATA ls_form_helper_config TYPE REF TO ts_form_helper.
            ls_form_helper_config ?= lr_helper.

            DATA(lv_form_name) = ls_form_helper_config->form_name.
            DATA(lv_report_name) = ls_form_helper_config->report_name.

            TRANSLATE lv_form_name TO UPPER CASE.
            TRANSLATE lv_report_name TO UPPER CASE.

            DATA(lv_name) = iv_name.
            DATA(lt_args) = it_args.
            DATA(ls_data) = is_data.

            PERFORM (lv_form_name) IN PROGRAM (lv_report_name)
              USING
                me
                lv_name
                lt_args
                ls_data
              CHANGING
                rs_result.

          WHEN OTHERS.
            rs_result-error = |Unsupported helper type { lv_type_name }|.
            RETURN.
        ENDCASE.

      CATCH cx_root INTO DATA(lx_error).
        rs_result-error = lx_error->get_longtext( ).
    ENDTRY.
  ENDMETHOD.


  METHOD backend_get_token_property.
    IF ir_struct IS BOUND.
      DATA(lv_kind) = me->backend_get_data_kind( ir_struct ).

      " Make sure the passed data is a struct, because on simple data like integer it would crash
      IF lv_kind = e_backend_data_kind_struct.
        DATA ls_token_base TYPE ts_parser_stmt_base.
        MOVE-CORRESPONDING ir_struct->* TO ls_token_base.

        IF sy-subrc = 0.
          rs_token = ls_token_base-token.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
