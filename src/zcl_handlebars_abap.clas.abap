" HandlebarsABAP {{version}} - https://github.com/monstermichl/HandlebarsABAP
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

    TYPES: BEGIN OF ts_partial,
             name            TYPE string,
             template_string TYPE string,
             partial         TYPE REF TO zcl_handlebars_abap,
           END OF ts_partial.

    TYPES: BEGIN OF ts_is_truthy_result,
             truthy TYPE abap_bool,
             error  TYPE string,
           END OF ts_is_truthy_result.

    TYPES: tr_data TYPE REF TO data.

    TYPES: tt_data TYPE STANDARD TABLE OF REF TO data WITH EMPTY KEY.

    TYPES: BEGIN OF ts_hash,
             key  TYPE string,
             data TYPE REF TO data,
           END OF ts_hash.

    TYPES: tt_hashes TYPE STANDARD TABLE OF ts_hash WITH KEY key.

    TYPES: BEGIN OF ts_options,
             instance TYPE REF TO zcl_handlebars_abap,
             name     TYPE string,
             args     TYPE tt_data,
             hashes   TYPE tt_hashes,
             data     TYPE tr_data,
           END OF ts_options.

    CONSTANTS: c_version TYPE string VALUE '1.0.1' ##NEEDED.

    "! Compiles the passed Handlebars template.
    "!
    "! @parameter iv_template_string | Handebars template string.
    CLASS-METHODS compile
      IMPORTING
        VALUE(iv_template_string) TYPE string
      RETURNING
        VALUE(rs_result)          TYPE ts_compile_result.

    CLASS-METHODS register_partial_static
      IMPORTING
        iv_name            TYPE string
        iv_template_string TYPE string
      RETURNING
        VALUE(rv_error)    TYPE string.

    "! Registers a helper method globally. The passed method must implement the following signature.
    "!
    "! METHODS helper_method
    "!   IMPORTING
    "!     VALUE(it_args)    TYPE zcl_handlebars_abap=>tt_data
    "!     VALUE(is_options) TYPE zcl_handlebars_abap=>ts_helper_options
    "!   RETURNING
    "!     VALUE(rs_result)  TYPE zcl_handlebars_abap=>ts_text_result.
    "!
    "! @parameter iv_name   | Helper name.
    "! @parameter ir_helper | Helper configuration (ts_class_helper | ts_object_helper | ts_func_module_helper | ts_form_helper).
    CLASS-METHODS register_helper_static
      IMPORTING
        iv_name         TYPE string
        ir_helper       TYPE any
      RETURNING
        VALUE(rv_error) TYPE string.

    METHODS register_partial
      IMPORTING
        iv_name            TYPE string
        iv_template_string TYPE string
      RETURNING
        VALUE(rv_error)    TYPE string.

    "! Registers a helper method. The method must implement the following signature.
    "!
    "! METHODS helper_method
    "!   IMPORTING
    "!     VALUE(it_args)    TYPE zcl_handlebars_abap=>tt_data
    "!     VALUE(is_options) TYPE zcl_handlebars_abap=>ts_helper_options
    "!   RETURNING
    "!     VALUE(rs_result)  TYPE zcl_handlebars_abap=>ts_text_result.
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
    "! @parameter ia_data | A struct or table.
    METHODS template
      IMPORTING
        ia_data          TYPE any OPTIONAL
      RETURNING
        VALUE(rs_result) TYPE ts_template_result.

    "! Renders the current block's content.
    "!
    "! @parameter ia_data | Data that shall be available within the block. The first entry is considered as 'this'.
    METHODS fn
      IMPORTING
        ia_data          TYPE any OPTIONAL
      RETURNING
        VALUE(rs_result) TYPE ts_text_result.

    "! Renders the current block's else-content.
    "!
    "! @parameter ia_data | Data that shall be available within the block. The first entry is considered as 'this'.
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
    TYPES: tt_partials TYPE TABLE OF ts_partial.
    TYPES: tt_helpers TYPE TABLE OF ts_helper.

    TYPES: BEGIN OF ts_find_partial_result,
             partial TYPE REF TO ts_partial,
             error   TYPE string,
           END OF ts_find_partial_result.

    TYPES: BEGIN OF ts_find_helper_result,
             helper TYPE REF TO ts_helper,
             error  TYPE string,
           END OF ts_find_helper_result.

    TYPES: BEGIN OF ts_get_data_type_result,
             name   TYPE string,
             is_ref TYPE abap_bool,
           END OF ts_get_data_type_result.

    CLASS-DATA: cr_helper_instance TYPE REF TO zcl_handlebars_abap.

    DATA: mt_partials TYPE tt_partials,
          mt_helpers  TYPE tt_helpers.

    CLASS-METHODS compile_internal
      IMPORTING
        VALUE(iv_template_string) TYPE string
        VALUE(iv_import_static)   TYPE abap_bool
      RETURNING
        VALUE(rs_result)          TYPE ts_compile_result.

    CLASS-METHODS try_to_load_template
      IMPORTING
        iv_name                   TYPE string
      RETURNING
        VALUE(rv_template_string) TYPE string.

    CLASS-METHODS get_instance
      RETURNING
        VALUE(rr_instance) TYPE REF TO zcl_handlebars_abap.

    CLASS-METHODS register_partial_internal
      IMPORTING
        ir_instance        TYPE REF TO zcl_handlebars_abap
        iv_name            TYPE string
        iv_template_string TYPE string
      RETURNING
        VALUE(rv_error)    TYPE string.

    CLASS-METHODS register_helper_internal
      IMPORTING
        ir_instance     TYPE REF TO zcl_handlebars_abap
        iv_name         TYPE string
        ir_helper       TYPE any
      RETURNING
        VALUE(rv_error) TYPE string.

    CLASS-METHODS find_partial
      IMPORTING
        ir_instance      TYPE REF TO zcl_handlebars_abap
        iv_name          TYPE string
      RETURNING
        VALUE(rs_result) TYPE ts_find_partial_result.

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
        iv_import_static_helpers  TYPE abap_bool
        iv_import_static_partials TYPE abap_bool.

    " .:: Tokenizer section.
    TYPES: e_tokenizer_token_type TYPE string.

    CONSTANTS: e_token_type_unknown         TYPE e_tokenizer_token_type VALUE 'unknown',
               e_token_type_text            TYPE e_tokenizer_token_type VALUE 'text',
               e_token_type_hashtag         TYPE e_tokenizer_token_type VALUE 'hashtag',
               e_token_type_slash           TYPE e_tokenizer_token_type VALUE 'slash',
               e_token_type_o_round_bracket TYPE e_tokenizer_token_type VALUE 'opening round bracket',
               e_token_type_c_round_bracket TYPE e_tokenizer_token_type VALUE 'closing round bracket',
               e_token_type_pipe            TYPE e_tokenizer_token_type VALUE 'pipe',
               e_token_type_at              TYPE e_tokenizer_token_type VALUE 'at',
               e_token_type_greater         TYPE e_tokenizer_token_type VALUE 'greater',
               e_token_type_equal           TYPE e_tokenizer_token_type VALUE 'equal',
               e_token_type_else            TYPE e_tokenizer_token_type VALUE 'else',
               e_token_type_as              TYPE e_tokenizer_token_type VALUE 'as',
               e_token_type_null            TYPE e_tokenizer_token_type VALUE 'null',
               e_token_type_undefined       TYPE e_tokenizer_token_type VALUE 'undefined',
               e_token_type_bool_literal    TYPE e_tokenizer_token_type VALUE 'bool literal',
               e_token_type_number_literal  TYPE e_tokenizer_token_type VALUE 'number literal',
               e_token_type_string_literal  TYPE e_tokenizer_token_type VALUE 'string literal',
               e_token_type_path            TYPE e_tokenizer_token_type VALUE 'path',
               e_token_type_hash_key        TYPE e_tokenizer_token_type VALUE 'hash key',
               e_token_type_space           TYPE e_tokenizer_token_type VALUE 'space',
               e_token_type_eop             TYPE e_tokenizer_token_type VALUE 'end of placeholder',
               e_token_type_eof             TYPE e_tokenizer_token_type VALUE 'end of file'.

    TYPES: tt_tokenizer_token_types TYPE STANDARD TABLE OF e_tokenizer_token_type WITH EMPTY KEY.

    TYPES: BEGIN OF ts_tokenizer_placeholder,
             offset     TYPE i,
             length     TYPE i,
             content    TYPE string,
             is_comment TYPE abap_bool,
           END OF ts_tokenizer_placeholder.

    TYPES: tt_tokenizer_placeholders TYPE STANDARD TABLE OF ts_tokenizer_placeholder WITH KEY offset.

    TYPES: BEGIN OF ts_tokenizer_token,
             position TYPE i,
             value    TYPE string,
             type     TYPE e_tokenizer_token_type,
           END OF ts_tokenizer_token.

    TYPES: tt_tokenizer_tokens TYPE STANDARD TABLE OF ts_tokenizer_token WITH KEY position.

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
        VALUE(iv_type)     TYPE e_tokenizer_token_type
      CHANGING
        VALUE(ct_tokens)   TYPE tt_tokenizer_tokens.

    " .:: Parser section.
    TYPES: tr_parser_statement TYPE REF TO data.

    TYPES: tt_parser_statements TYPE STANDARD TABLE OF tr_parser_statement WITH EMPTY KEY.

    TYPES: tr_parser_expression TYPE tr_parser_statement.

    TYPES: tt_parser_expressions TYPE STANDARD TABLE OF tr_parser_expression WITH EMPTY KEY.

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

    TYPES: BEGIN OF ts_parser_null_literal,
             value TYPE string.
             INCLUDE TYPE ts_parser_stmt_base.
    TYPES: END OF ts_parser_null_literal.

    TYPES: BEGIN OF ts_parser_undefined_literal,
             value TYPE string.
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

    TYPES: BEGIN OF ts_parser_hash,
             key        TYPE string,
             expression TYPE tr_parser_expression,
           END OF ts_parser_hash.

    TYPES: tt_parser_hashes TYPE STANDARD TABLE OF ts_parser_hash WITH KEY key.

    TYPES: BEGIN OF ts_parser_partial,
             name    TYPE tr_parser_expression,
             context TYPE tr_parser_expression,
             hashes  TYPE tt_parser_hashes.
             INCLUDE TYPE ts_parser_stmt_base.
    TYPES: END OF ts_parser_partial.

    TYPES: BEGIN OF ts_parser_helper,
             name   TYPE string,
             args   TYPE tt_parser_expressions,
             hashes TYPE tt_parser_hashes.
             INCLUDE TYPE ts_parser_stmt_base.
    TYPES: END OF ts_parser_helper.

    TYPES: BEGIN OF ts_parser_block_param,
             name TYPE string.
             INCLUDE    TYPE ts_parser_stmt_base.
    TYPES: END OF ts_parser_block_param.

    TYPES: tt_parser_block_params TYPE STANDARD TABLE OF ts_parser_block_param WITH EMPTY KEY.

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
             hashes      TYPE tt_parser_hashes,
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

    METHODS parser_eval_partial
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
    TYPES: e_backend_data_kinds TYPE string.

    CONSTANTS: e_backend_data_kind_unknown   TYPE e_backend_data_kinds VALUE 'unknown',
               e_backend_data_kind_undefined TYPE e_backend_data_kinds VALUE 'undefined',
               e_backend_data_kind_simple    TYPE e_backend_data_kinds VALUE 'simple',
               e_backend_data_kind_struct    TYPE e_backend_data_kinds VALUE 'struct',
               e_backend_data_kind_table     TYPE e_backend_data_kinds VALUE 'table'.

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

    TYPES: tt_backend_block_args TYPE STANDARD TABLE OF ts_backend_block_arg WITH EMPTY KEY.

    TYPES: BEGIN OF ts_backend_block_stack_block,
             block  TYPE REF TO ts_parser_block,
             args   TYPE tt_backend_block_args,
             pseudo TYPE abap_bool,
           END OF ts_backend_block_stack_block.

    TYPES: tt_backend_block_stack TYPE TABLE OF ts_backend_block_stack_block.

    DATA: mt_backend_block_stack   TYPE tt_backend_block_stack,
          mv_backend_inline_helper TYPE ts_parser_inline_helper.

    METHODS template_internal
      IMPORTING
        ir_data          TYPE REF TO data OPTIONAL
        it_block_stack   TYPE tt_backend_block_stack OPTIONAL
      RETURNING
        VALUE(rs_result) TYPE ts_template_result.

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
        ir_data          TYPE tr_data OPTIONAL
      RETURNING
        VALUE(rs_result) TYPE ts_text_result.

    METHODS backend_eval_stmt
      IMPORTING
        ir_stmt          TYPE REF TO data
        ir_data          TYPE tr_data
      RETURNING
        VALUE(rs_result) TYPE ts_text_result.

    METHODS backend_eval_expr
      IMPORTING
        ir_stmt          TYPE REF TO data
        ir_data          TYPE tr_data OPTIONAL
      RETURNING
        VALUE(rs_result) TYPE ts_backend_eval_expr_result.

    METHODS backend_eval_literal_expr
      IMPORTING
        ir_stmt          TYPE REF TO data
        ir_data          TYPE tr_data OPTIONAL ##NEEDED
      RETURNING
        VALUE(rs_result) TYPE ts_backend_eval_expr_result.

    METHODS backend_eval_partial
      IMPORTING
        ir_partial       TYPE REF TO ts_parser_partial
        ir_data          TYPE tr_data
      RETURNING
        VALUE(rs_result) TYPE ts_text_result.

    METHODS backend_eval_helper
      IMPORTING
        ir_helper        TYPE REF TO data
        ir_data          TYPE tr_data
      RETURNING
        VALUE(rs_result) TYPE ts_text_result.

    METHODS backend_eval_block
      IMPORTING
        ir_block         TYPE REF TO ts_parser_block
        ir_data          TYPE tr_data
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
        VALUE(it_args)    TYPE tt_data ##NEEDED
        VALUE(is_options) TYPE ts_options
      RETURNING
        VALUE(rs_result)  TYPE ts_text_result.

    METHODS backend_eval_each_helper
      IMPORTING
        VALUE(it_args)    TYPE tt_data ##NEEDED
        VALUE(is_options) TYPE ts_options
      RETURNING
        VALUE(rs_result)  TYPE ts_text_result.

    METHODS backend_eval_with_helper
      IMPORTING
        VALUE(it_args)    TYPE tt_data ##NEEDED
        VALUE(is_options) TYPE ts_options
      RETURNING
        VALUE(rs_result)  TYPE ts_text_result.

    METHODS backend_eval_inline_helper
      IMPORTING
        ir_inline_helper TYPE REF TO ts_parser_inline_helper
        ir_data          TYPE tr_data
      RETURNING
        VALUE(rs_result) TYPE ts_text_result.

    METHODS backend_eval_log_helper
      IMPORTING
        VALUE(it_args)    TYPE tt_data ##NEEDED
        VALUE(is_options) TYPE ts_options
      RETURNING
        VALUE(rs_result)  TYPE ts_text_result.

    METHODS backend_eval_sub_expr
      IMPORTING
        ir_sub_expr      TYPE REF TO ts_parser_sub_expr
        ir_data          TYPE tr_data
      RETURNING
        VALUE(rs_result) TYPE ts_backend_path_eval_result.

    METHODS backend_eval_path
      IMPORTING
        ir_path          TYPE REF TO ts_parser_path
        ir_data          TYPE tr_data
      RETURNING
        VALUE(rs_result) TYPE ts_backend_path_eval_result.

    METHODS backend_get_data_kind
      IMPORTING
        ir_data        TYPE REF TO data
      RETURNING
        VALUE(rv_kind) TYPE e_backend_data_kinds.

    METHODS backend_push_block
      IMPORTING
        VALUE(is_block) TYPE ts_backend_block_stack_block.

    METHODS backend_push_pseudo_block
      IMPORTING
        VALUE(ir_data) TYPE REF TO data.

    METHODS backend_pop_block
      RETURNING
        VALUE(rs_block) TYPE ts_backend_block_stack_block.

    METHODS backend_get_block
      IMPORTING
        VALUE(iv_index)    TYPE i
      EXPORTING
        VALUE(er_block)    TYPE REF TO ts_backend_block_stack_block
        VALUE(ev_fallback) TYPE abap_bool.

    METHODS backend_get_last_block
      RETURNING
        VALUE(rr_block) TYPE REF TO ts_backend_block_stack_block.

    METHODS backend_call_helper
      IMPORTING
        iv_name          TYPE string
        it_args          TYPE tt_data OPTIONAL
        it_hashes        TYPE tt_hashes OPTIONAL
        ir_data          TYPE tr_data
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
    rs_result = zcl_handlebars_abap=>compile_internal(
      iv_template_string = iv_template_string
      iv_import_static   = abap_true
    ).
  ENDMETHOD.


  METHOD register_partial_static.
    rv_error = zcl_handlebars_abap=>register_partial_internal(
      ir_instance        = zcl_handlebars_abap=>get_instance( )
      iv_name            = iv_name
      iv_template_string = iv_template_string
    ).
  ENDMETHOD.


  METHOD register_helper_static.
    rv_error = zcl_handlebars_abap=>register_helper_internal(
      ir_instance = zcl_handlebars_abap=>get_instance( )
      iv_name     = iv_name
      ir_helper   = ir_helper
    ).
  ENDMETHOD.


  METHOD register_partial.
    rv_error = zcl_handlebars_abap=>register_partial_internal(
      ir_instance        = me
      iv_name            = iv_name
      iv_template_string = iv_template_string
    ).
  ENDMETHOD.


  METHOD register_helper.
    rv_error = zcl_handlebars_abap=>register_helper_internal(
      ir_instance = me
      iv_name     = iv_name
      ir_helper   = ir_helper
    ).
  ENDMETHOD.


  METHOD template.
    DATA(lr_data) = me->any_to_ref_to_data( ia_data ).

    " Push a pseudo block to have a base for the whole template.
    me->backend_push_pseudo_block( lr_data ).

    " Template.
    rs_result = me->template_internal( ir_data = lr_data ).

    " Pop pseudo block.
    me->backend_pop_block( ).
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


  METHOD compile_internal.
    DATA(lv_template_string) = iv_template_string.
    DATA(lo_template) = NEW zcl_handlebars_abap(
      iv_import_static_helpers  = iv_import_static
      iv_import_static_partials = iv_import_static
    ).

    " First, try to load stored HTML template from SMW0.
    lv_template_string = zcl_handlebars_abap=>try_to_load_template( lv_template_string ).

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


  METHOD template_internal.

    " Add provided block stack.
    LOOP AT it_block_stack INTO DATA(ls_block).
      me->backend_push_block( ls_block ).
    ENDLOOP.

    DATA(ls_result) = me->backend_eval_stmt(
      ir_stmt = me->mr_template
      ir_data = ir_data
    ).
    DATA(lv_error) = ls_result-error.

    IF lv_error IS NOT INITIAL.
      rs_result-error = lv_error.
      RETURN.
    ENDIF.

    rs_result-text = ls_result-text.
  ENDMETHOD.


  METHOD try_to_load_template.
    rv_template_string = iv_name.

    IF strlen( iv_name ) <= 40.
      DATA: lt_types   TYPE string_table,
            ls_datatab TYPE wwwdatatab.

      " Add WWWDATA types based on priority (binary data preferred).
      APPEND 'MI' TO lt_types.
      APPEND 'HT' TO lt_types.

      ls_datatab-objid = iv_name.

      LOOP AT lt_types INTO DATA(lv_type).
        DATA: lt_w3html TYPE TABLE OF w3html,
              lt_w3mime TYPE TABLE OF w3mime.

        CLEAR lt_w3html.
        CLEAR lt_w3mime.

        ls_datatab-relid = lv_type.

        CALL FUNCTION 'WWWDATA_IMPORT'
          EXPORTING
            key               = ls_datatab
          TABLES
            html              = lt_w3html
            mime              = lt_w3mime
          EXCEPTIONS
            wrong_object_type = 1
            import_error      = 2
            OTHERS            = 3.

        " If loading the template was successful, use based on if it's binary or HTML data.
        IF lt_w3mime[] IS NOT INITIAL.

          " Convert binary string to UTF-8 string.
          DATA(lv_xstring) = cl_bcs_convert=>solix_to_xstring( lt_w3mime ).
          CLEAR rv_template_string.

          cl_abap_conv_in_ce=>create( input = lv_xstring encoding = 'UTF-8' )->read( IMPORTING data = rv_template_string ).
        ELSEIF lt_w3html[] IS NOT INITIAL.
          TRY.
              rv_template_string = cl_bcs_convert=>raw_to_string( lt_w3html ).
            CATCH cx_root.
              " Nothing to do. Template string stays as it is.
          ENDTRY.
        ELSE.
          CONTINUE.
        ENDIF.

        EXIT.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD get_instance.
    IF zcl_handlebars_abap=>cr_helper_instance IS NOT BOUND.
      zcl_handlebars_abap=>cr_helper_instance = NEW zcl_handlebars_abap(
        iv_import_static_helpers  = abap_false
        iv_import_static_partials = abap_false
      ).
    ENDIF.

    rr_instance = zcl_handlebars_abap=>cr_helper_instance.
  ENDMETHOD.


  METHOD register_partial_internal.
    IF iv_name IS INITIAL.
      rv_error = 'No partial name provided'.
      RETURN.
    ELSEIF iv_template_string IS INITIAL.
      rv_error = 'No partial template string provided'.
      RETURN.
    ENDIF.

    DATA(ls_partial_result) = zcl_handlebars_abap=>compile_internal(
      iv_template_string = iv_template_string
      iv_import_static   = abap_false
    ).
    DATA(lv_partial_compile_error) = ls_partial_result-error.

    IF lv_partial_compile_error IS NOT INITIAL.
      rv_error = lv_partial_compile_error.
      RETURN.
    ENDIF.

    DATA(ls_find_partial_result) = ir_instance->find_partial( ir_instance = ir_instance iv_name = iv_name ).
    DATA(lr_partial_instance) = ls_partial_result-instance.

    " If a corresponding partial was found, update it.
    IF ls_find_partial_result-error IS INITIAL.
      ls_find_partial_result-partial->partial ?= lr_partial_instance.
      ls_find_partial_result-partial->template_string = iv_template_string.
    ELSE.
      APPEND VALUE ts_partial(
        name            = iv_name
        template_string = iv_template_string
        partial         = lr_partial_instance
      ) TO ir_instance->mt_partials.
    ENDIF.
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

    DATA(ls_find_helper_result) = ir_instance->find_helper( ir_instance = ir_instance iv_name = iv_name ).

    " If a corresponding helper was found, update it.
    IF ls_find_helper_result-error IS INITIAL.
      ls_find_helper_result-helper->helper ?= lr_helper.
    ELSE.
      APPEND VALUE ts_helper( name = iv_name helper = lr_helper ) TO ir_instance->mt_helpers.
    ENDIF.
  ENDMETHOD.


  METHOD find_partial.
    " Try to get a registered partial by its name
    READ TABLE ir_instance->mt_partials REFERENCE INTO DATA(lr_partial) WITH KEY name = iv_name.

    IF sy-subrc <> 0.
      rs_result-error = |No partial found for { iv_name }|.
      RETURN.
    ENDIF.

    rs_result-partial = lr_partial.
  ENDMETHOD.


  METHOD find_helper.
    " Try to get a registered helper by its name
    READ TABLE ir_instance->mt_helpers REFERENCE INTO DATA(lr_helper) WITH KEY name = iv_name.

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
    IF iv_import_static_helpers = abap_true OR iv_import_static_partials = abap_true.
      DATA(lr_helper_instance) = zcl_handlebars_abap=>get_instance( ).

      " Add globally registered helpers.
      IF iv_import_static_helpers = abap_true.
        LOOP AT lr_helper_instance->mt_helpers INTO DATA(ls_helper).
          me->register_helper( iv_name = ls_helper-name ir_helper = ls_helper-helper ).
        ENDLOOP.
      ENDIF.

      " Add globally registered partials.
      IF iv_import_static_partials = abap_true.
        LOOP AT lr_helper_instance->mt_partials INTO DATA(ls_partial).
          me->register_partial( iv_name = ls_partial-name iv_template_string = ls_partial-template_string ).
        ENDLOOP.
      ENDIF.
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
             type    TYPE e_tokenizer_token_type,
           END OF ts_tokenizer_token_mapping.

    TYPES: tt_token_mappings TYPE STANDARD TABLE OF ts_tokenizer_token_mapping WITH KEY pattern.

    TYPES: BEGIN OF ts_match_mapping,
             match TYPE match_result,
             type  TYPE e_tokenizer_token_type,
           END OF ts_match_mapping.

    CONSTANTS: c_space           TYPE string VALUE '\s+',
               c_hashtag         TYPE string VALUE '\#',
               c_slash           TYPE string VALUE '\/',
               c_equal           TYPE string VALUE '\=',
               c_o_round_bracket TYPE string VALUE '\(',
               c_c_round_bracket TYPE string VALUE '\)',
               c_pipe            TYPE string VALUE '\|',
               c_at              TYPE string VALUE '\@',
               c_greater         TYPE string VALUE '>',
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
      ( pattern = c_greater         type = e_token_type_greater         )
      ( pattern = c_equal           type = e_token_type_equal           )
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

      DATA lt_temporary_tokens TYPE tt_tokenizer_tokens.
      CLEAR lt_temporary_tokens.

      lv_text_length = lv_placeholder_offset - lv_previous_offset.
      lv_text = iv_template_string+lv_previous_offset(lv_text_length).

      IF lv_text_length > 0.
        me->tokenizer_add_token(
          EXPORTING
            iv_value  = lv_text iv_position = lv_previous_offset iv_type = e_token_type_text
          CHANGING
            ct_tokens = lt_temporary_tokens
        ).
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
            me->tokenizer_add_token(
              EXPORTING
                iv_value    = lv_collected_string
                iv_position = lv_token_position
                iv_type     = e_token_type_string_literal
              CHANGING
                ct_tokens   = lt_temporary_tokens
            ).
            lv_collecting_string_before = lv_collecting_string.
          ENDIF.
        ELSE.
          DATA: ls_match         TYPE match_result,
                ls_match_mapping TYPE ts_match_mapping.

          CLEAR ls_match_mapping.

          " Try to find characters.
          LOOP AT lt_char_mappings INTO DATA(ls_mapping).
            FIND REGEX |^({ ls_mapping-pattern })| IN lv_subcontent RESULTS ls_match.

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

              FIND REGEX lv_pattern IN lv_subcontent RESULTS ls_match.

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

            me->tokenizer_add_token(
              EXPORTING
                iv_value    = lv_part
                iv_position = lv_token_position
                iv_type     = lv_type
              CHANGING
                ct_tokens   = lt_temporary_tokens
            ).
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

      DATA(lv_temp_token_index) = 0.
      DATA(lv_temp_tokens_length) = lines( lt_temporary_tokens ).

      " After all tokens were collected, do some post-processing.
      WHILE lv_temp_token_index < lv_temp_tokens_length.
        lv_temp_token_index = lv_temp_token_index + 1.

        DATA(ls_temp_token) = lt_temporary_tokens[ lv_temp_token_index ].
        DATA(lv_add) = abap_true.

        CASE ls_temp_token-type.
            " If path was found evaluate if it's a hash-argument.
          WHEN e_token_type_path.
            " Make sure it's just a single word.
            FIND REGEX '\w+' IN ls_temp_token-value.

            IF sy-subrc = 0.
              DATA: ls_next_token            TYPE ts_tokenizer_token,
                    ls_next_after_next_token TYPE ts_tokenizer_token.

              CLEAR ls_next_token.
              CLEAR ls_next_after_next_token.

              READ TABLE lt_temporary_tokens INDEX lv_temp_token_index + 1 INTO ls_next_token.
              READ TABLE lt_temporary_tokens INDEX lv_temp_token_index + 2 INTO ls_next_after_next_token.

              " If next token is equal and afterwards is no space, it's most probably a hash argument.
              IF (
                ls_next_token            IS NOT INITIAL AND ls_next_token-type            =  e_token_type_equal AND
                ls_next_after_next_token IS NOT INITIAL AND ls_next_after_next_token-type <> e_token_type_space
              ).
                " Change token type to hash key and skip assign token.
                ls_temp_token-type  = e_token_type_hash_key.
                lv_temp_token_index = lv_temp_token_index + 1. " Skip assign token.
              ENDIF.
            ENDIF.
          WHEN e_token_type_space.
            " Skip spaces.
            lv_add = abap_false.
        ENDCASE.

        IF lv_add = abap_true.
          APPEND ls_temp_token TO me->mt_tokenizer_tokens.
        ENDIF.
      ENDWHILE.

      " Add EOP (end-of-placeholder) token at the end of a placeholder.
      me->tokenizer_add_token(
        EXPORTING
          iv_value    = ''
          iv_position = lv_token_position
          iv_type     = e_token_type_eop
        CHANGING
          ct_tokens   = me->mt_tokenizer_tokens
      ).
    ENDLOOP.

    lv_text = iv_template_string+lv_previous_offset.
    lv_text_length = strlen( lv_text ).

    IF lv_text_length > 0.
      me->tokenizer_add_token(
        EXPORTING
          iv_value = lv_text
          iv_position = lv_previous_offset
          iv_type = e_token_type_text
        CHANGING
          ct_tokens   = me->mt_tokenizer_tokens
      ).
    ENDIF.

    " Terminate with EOF-token.
    me->tokenizer_add_token(
      EXPORTING
        iv_value    = ''
        iv_position = -1
        iv_type     = e_token_type_eof
      CHANGING
        ct_tokens   = me->mt_tokenizer_tokens
    ).
  ENDMETHOD.


  METHOD tokenizer_eval_placeholders.
    TYPES: e_comment_types TYPE i.

    CONSTANTS: e_comment_type_none    TYPE e_comment_types VALUE 0,
               e_comment_type_simple  TYPE e_comment_types VALUE 1,
               e_comment_type_complex TYPE e_comment_types VALUE 2.

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

        DATA ls_placeholder TYPE ts_tokenizer_placeholder.
        CLEAR ls_placeholder.

        ls_placeholder-offset     = lv_offset.
        ls_placeholder-length     = lv_i - lv_offset.
        ls_placeholder-content    = iv_template_string+lv_start_index(lv_length).
        ls_placeholder-is_comment = COND abap_bool(
          WHEN lv_comment_type <> e_comment_type_none THEN abap_true
          ELSE abap_false
        ).

        APPEND ls_placeholder TO rt_placeholders.

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
    APPEND VALUE #( value = iv_value position = iv_position type = iv_type ) TO ct_tokens.
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
    DATA: ls_result TYPE ts_parser_eval_result,
          lv_error  TYPE string.

    DATA(ls_token) = me->parser_peek( ).
    DATA(ls_next_token) = me->parser_peek_at( 1 ).
    DATA(lv_valid) = abap_true.
    DATA(lv_expect_eop) = abap_true.

    CASE ls_token-type.
      WHEN e_token_type_unknown.
        lv_error = me->parser_build_error( iv_error = |Unknown token type| is_token = ls_token ).

        " If the current token is a > it's a partial.
      WHEN e_token_type_greater.
        CASE ls_next_token-type.
          WHEN e_token_type_path OR e_token_type_o_round_bracket.
            ls_result = me->parser_eval_partial( ).

          WHEN OTHERS.
            lv_valid = abap_false.
        ENDCASE.

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

    IF lv_error IS INITIAL.
      lv_error = rs_result-error.
    ENDIF.

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

      READ TABLE lt_termination_tokens TRANSPORTING NO FIELDS WITH KEY table_line = ls_token-type.

      " Check if the current token is a termination token.
      IF sy-subrc = 0.
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


  METHOD parser_eval_partial.
    DATA(ls_token) = me->parser_eat( ).
    DATA(ls_start_token) = ls_token.

    " Make sure block starts with >.
    IF ls_token-type <> e_token_type_greater.
      rs_result-error = me->parser_build_expected_error( iv_error = '>' is_token = ls_token ).
      RETURN.
    ENDIF.

    ls_token = me->parser_peek( ).
    DATA(lr_partial) = NEW ts_parser_partial( token = ls_token ).

    CASE ls_token-type.
      WHEN e_token_type_path.
        DATA(ls_eval_path_result) = me->parser_eval_path( ).

        IF ls_eval_path_result-error IS NOT INITIAL.
          rs_result-error = ls_eval_path_result-error.
          RETURN.
        ENDIF.

        DATA lr_path TYPE REF TO ts_parser_path.
        lr_path ?= ls_eval_path_result-stmt.

        " Make sure parsed path is a single identifier.
        IF lr_path->is_identifier = abap_false.
          rs_result-error = me->parser_build_expected_error( iv_error = 'identifier' is_token = ls_token ).
          RETURN.
        ENDIF.

        lr_partial->name = lr_path.

      WHEN e_token_type_o_round_bracket.
        DATA(ls_subexpression_result) = me->parser_eval_sub_expr( ).

        IF ls_subexpression_result-error IS NOT INITIAL.
          rs_result-error = ls_subexpression_result-error.
          RETURN.
        ENDIF.

        lr_partial->name = ls_subexpression_result-stmt.

      WHEN OTHERS.
        rs_result-error = me->parser_build_expected_error( iv_error = 'partial name or subexpression' is_token = ls_token ).
        RETURN.
    ENDCASE.

    ls_token = me->parser_peek( ).
    DATA(ls_arguments_result) = me->parser_eval_args( ).

    IF ls_arguments_result-error IS NOT INITIAL.
      rs_result-error = ls_arguments_result-error.
      RETURN.
    ENDIF.

    DATA(lt_args) = ls_arguments_result-expressions.
    DATA(lv_length_args) = lines( lt_args ).

    IF lv_length_args > 0.
      IF lv_length_args > 1.
        rs_result-error = me->parser_build_error( iv_error = 'Only one context argument can be passed to partial' is_token = ls_token ).
        RETURN.
      ENDIF.

      lr_partial->context = lt_args[ 1 ].
    ENDIF.

    lr_partial->hashes = ls_arguments_result-hashes.
    rs_result-stmt = lr_partial.
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
    DATA(lt_hashes) = ls_eval_args_result-hashes.

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

    " Create instance of ts_parser_block on the heap.
    DATA(ls_block) = NEW ts_parser_block(
      name   = lv_start_helper_name
      args   = lt_args
      hashes = lt_hashes
      body   = VALUE ts_parser_body( )
      params = lt_params
      token  = ls_start_token
    ).

    " Only evaluate block content if not termination token was found.
    IF ls_token-type <> e_token_type_slash.
      " Evaluate block-statements.
      DATA(ls_stmts_result) = me->parser_eval_stmts(
        it_termination_token_types = VALUE tt_tokenizer_token_types( ( e_token_type_else ) ( e_token_type_slash ) ) " Terminate on else and /.
      ).
      lv_error = ls_stmts_result-error.

      IF lv_error IS NOT INITIAL.
        rs_result-error = lv_error.
        RETURN.
      ENDIF.

      ls_block->body = VALUE ts_parser_body( statements = ls_stmts_result-stmts token = ls_token ).
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
      name   = ls_eval_helper_result-name
      args   = ls_eval_args_result-expressions
      hashes = ls_eval_args_result-hashes
      token  = ls_token
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
        DATA(lv_bool_value) = COND abap_bool(
          WHEN lv_value <> c_false THEN abap_true
          ELSE abap_false
        ).
        lr_data = NEW ts_parser_bool_literal( value = lv_bool_value token = ls_token ).

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
            DATA(ls_next_token) = me->parser_peek_at( 1 ).

            " Make sure EOP is part of termination token list.
            APPEND e_token_type_eop TO lt_term_token_types.

            READ TABLE lt_term_token_types TRANSPORTING NO FIELDS WITH KEY table_line = ls_next_token-type.

            " If the next token is not in termination token list, it's an inline-helper.
            IF sy-subrc <> 0.
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

    CONSTANTS c_dot TYPE string VALUE '.'.
    DATA(lv_value) = ls_token-value.

    " If path is a single dot, replace it by "this".
    IF lv_value = c_dot.
      lv_value = c_this.
    ENDIF.

    " First split at slashes.
    SPLIT lv_value AT '/' INTO TABLE DATA(lt_relative_parts).

    LOOP AT lt_relative_parts INTO DATA(lv_relative_part).
      IF lv_relative_part = c_relative.
        APPEND lv_relative_part TO lt_collected_parts.
      ELSE.

        " Now split at single dots.
        SPLIT lv_relative_part AT c_dot INTO TABLE DATA(lt_parts).

        LOOP AT lt_parts INTO DATA(lv_part).
          APPEND lv_part TO lt_collected_parts.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

    DATA(lv_is_identifier) = COND abap_bool(
      WHEN lines( lt_collected_parts ) = 1 AND lt_collected_parts[ 1 ] <> c_relative THEN abap_true
      ELSE abap_false
    ).

    rs_result-stmt = NEW ts_parser_path(
      parts         = lt_collected_parts
      is_identifier = lv_is_identifier
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
    DATA ls_hash TYPE ts_parser_hash.
    DATA lt_hashes TYPE tt_parser_hashes.
    DATA(lt_termination_token_types) = it_termination_token_types.

    " Add safety net.
    APPEND e_token_type_eop TO lt_termination_token_types.
    APPEND e_token_type_eof TO lt_termination_token_types.

    " Create temporary termination tokens.
    DATA(lt_temp_term_token_types) = lt_termination_token_types.
    APPEND e_token_type_hash_key TO lt_temp_term_token_types.

    DO.
      DATA(ls_token) = me->parser_peek( ).
      READ TABLE lt_termination_token_types TRANSPORTING NO FIELDS WITH KEY table_line = ls_token-type.

      IF sy-subrc = 0.
        EXIT.
      ENDIF.

      IF ls_token-type = e_token_type_hash_key.
        IF ls_hash-key IS NOT INITIAL.
          rs_result-error = me->backend_build_error( iv_error = |Expected expression| is_token = ls_token ).
          RETURN.
        ENDIF.

        ls_hash-key = ls_token-value.
        me->parser_eat( ).
      ELSE.
        DATA(ls_expr_result) = me->parser_eval_expr( lt_temp_term_token_types ).
        DATA(lv_error) = ls_expr_result-error.

        IF lv_error IS NOT INITIAL.
          rs_result-error = lv_error.
          RETURN.
        ENDIF.

        DATA(lr_expr) = ls_expr_result-stmt.

        IF ls_hash-key IS NOT INITIAL.
          ls_hash-expression = lr_expr.
          APPEND ls_hash TO lt_hashes.
        ELSE.
          APPEND lr_expr TO lt_args.
        ENDIF.

        CLEAR ls_hash.
      ENDIF.
    ENDDO.

    rs_result-expressions = lt_args.
    rs_result-hashes      = lt_hashes.
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
        ir_data = ir_data
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
          ir_data  = ir_data
        ).

      WHEN 'ts_parser_text'.
        DATA lr_text TYPE REF TO ts_parser_text.
        lr_text ?= ir_stmt.

        rs_result-text = lr_text->value.

      WHEN 'ts_parser_partial'.
        DATA lr_partial TYPE REF TO ts_parser_partial.
        lr_partial ?= ir_stmt.

        rs_result = me->backend_eval_partial(
          ir_partial = lr_partial
          ir_data    = ir_data
        ).

      WHEN 'ts_parser_block'.
        DATA lr_block TYPE REF TO ts_parser_block.
        lr_block ?= ir_stmt.

        rs_result = me->backend_eval_block(
          ir_block = lr_block
          ir_data  = ir_data
        ).

      WHEN OTHERS.
        DATA(ls_eval_expr_result) = me->backend_eval_expr(
          ir_stmt = ir_stmt
          ir_data = ir_data
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
        ASSIGN ls_eval_expr_result-data->* TO FIELD-SYMBOL(<data>).
        rs_result-text = |{ <data> }|.
    ENDCASE.
  ENDMETHOD.


  METHOD backend_eval_expr.

    " First, try to evaluate if it's a literal.
    DATA(ls_literal_result) = me->backend_eval_literal_expr( ir_stmt = ir_stmt ir_data = ir_data ).

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
          ir_data     = ir_data
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
          ir_data          = ir_data
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
          ir_data = ir_data
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
    DATA lr_data TYPE REF TO data.

    DATA(lv_type) = me->get_data_type( ir_stmt ).
    DATA(lv_type_name) = lv_type-name.

    CASE lv_type_name.
      WHEN 'ts_parser_bool_literal'.
        lr_data = ir_stmt.

      WHEN 'ts_parser_float_literal'.
        lr_data = ir_stmt.

      WHEN 'ts_parser_string_literal'.
        lr_data = ir_stmt.

      WHEN 'ts_parser_null_literal' OR 'ts_parser_undefined_literal'.
        lr_data = ir_stmt.

      WHEN OTHERS.
        DATA(ls_token) = me->backend_get_token_property( ir_stmt ).

        rs_result-error = me->backend_build_error( iv_error = |Unknown literal type { lv_type_name }| is_token = ls_token ).
        RETURN.
    ENDCASE.

    ASSIGN lr_data->* TO FIELD-SYMBOL(<structure>).
    ASSIGN COMPONENT 'VALUE' OF STRUCTURE <structure> TO FIELD-SYMBOL(<value>).

    DATA(lo_descriptor) = CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_data( <value> ) ).

    DATA lr_literal_value TYPE REF TO data.
    CREATE DATA lr_literal_value TYPE HANDLE lo_descriptor.

    lr_literal_value->* = <value>.

    rs_result-data = lr_literal_value.
    rs_result-kind = me->backend_get_data_kind( lr_literal_value ).
  ENDMETHOD.


  METHOD backend_eval_partial.
    DATA(lr_name) = ir_partial->name.
    DATA(ls_token) = ir_partial->token.
    DATA(lv_type) = me->get_data_type( lr_name )-name.

    DATA lv_name TYPE string.

    CASE lv_type.
      WHEN 'ts_parser_path'.
        DATA lr_path TYPE REF TO ts_parser_path.

        lr_path ?= lr_name.
        lv_name = lr_path->parts[ 1 ].

      WHEN 'ts_parser_sub_expr'.
        DATA lr_sub_expr TYPE REF TO ts_parser_sub_expr.
        lr_path ?= lr_name.

        DATA(ls_sub_expr_result) = me->backend_eval_sub_expr(
          EXPORTING
            ir_sub_expr = lr_sub_expr
            ir_data     = ir_data
        ).
        DATA(lv_kind) = ls_sub_expr_result-kind.

        IF ls_sub_expr_result-error IS NOT INITIAL.
          rs_result-error = ls_sub_expr_result-error.
          RETURN.
        ELSEIF lv_kind <> e_backend_data_kind_simple.
          rs_result-error = me->backend_build_error(
            iv_error = 'Subexpression for partial returned no simple type'
            is_token = lr_sub_expr->token
          ).
          RETURN.
        ENDIF.

        lv_name = |{ ls_sub_expr_result-data->* }|.

      WHEN OTHERS.
        rs_result-error = me->backend_build_error( iv_error = |Unknown partial name type { lv_type }| is_token = ls_token ).
        RETURN.
    ENDCASE.

    DATA(lr_find_partial_result) = me->find_partial(
      EXPORTING
        ir_instance = me
        iv_name     = lv_name
    ).

    IF lr_find_partial_result-error IS NOT INITIAL.
      rs_result-error = lr_find_partial_result-error.
      RETURN.
    ENDIF.

    DATA(lr_context) = ir_partial->context.
    DATA(lr_data) = ir_data.

    IF lr_context IS BOUND.
      DATA(ls_context_result) = me->backend_eval_expr(
        ir_stmt = lr_context
        ir_data = lr_data
      ).

      IF ls_context_result-error IS NOT INITIAL.
        rs_result-error = ls_context_result-error.
        RETURN.
      ENDIF.

      lr_data = ls_context_result-data.
    ENDIF.

    DATA(lt_hashes) = ir_partial->hashes.

    " If hashes were provided, they must be merged with the current context.
    IF lines( lt_hashes ) > 0.
      DATA(ls_context_kind) = me->backend_get_data_kind( lr_data ).

      IF ls_context_kind <> e_backend_data_kind_struct.
        rs_result-error = me->backend_build_error( iv_error = 'Hash arguments can only be set on an object context' is_token = ls_token ).
        RETURN.
      ENDIF.

      DATA(ls_context_type) = me->get_data_type( lr_data ).
      DATA lo_struct_descriptor TYPE REF TO cl_abap_structdescr.

      IF ls_context_type-is_ref = abap_true.
        lo_struct_descriptor = CAST cl_abap_structdescr( cl_abap_datadescr=>describe_by_data_ref( lr_data ) ).
      ELSE.
        lo_struct_descriptor = CAST cl_abap_structdescr( cl_abap_datadescr=>describe_by_data( lr_data->* ) ).
      ENDIF.

      DATA: lr_hash_data           TYPE REF TO data,
            lt_original_properties TYPE string_table.

      DATA(lt_components) = lo_struct_descriptor->get_components( ).

      " Collect original properties.
      LOOP AT lt_components INTO DATA(ls_original_component).
        APPEND ls_original_component-name TO lt_original_properties.
      ENDLOOP.

      DATA(lv_new_context_created) = abap_false.

      " Create new type dynamically.
      LOOP AT lt_hashes INTO DATA(ls_hash).
        DATA ls_property_descriptor TYPE abap_componentdescr.
        CLEAR ls_property_descriptor.

        DATA(ls_hash_expr_result) = me->backend_eval_expr(
          ir_stmt = ls_hash-expression
          ir_data = lr_data
        ).

        IF ls_hash_expr_result-error IS NOT INITIAL.
          rs_result-error = ls_hash_expr_result-error.
          RETURN.
        ENDIF.

        DATA(lv_key) = ls_hash-key.
        TRANSLATE lv_key TO UPPER CASE.

        READ TABLE lt_components TRANSPORTING NO FIELDS WITH KEY name = lv_key.

        " If the property exists already, remove it from the freshly created type.
        IF sy-subrc = 0.
          DATA(lv_index) = sy-tabix.

          DELETE lt_components INDEX lv_index.
          DELETE lt_original_properties INDEX lv_index.
        ENDIF.

        lr_hash_data = ls_hash_expr_result-data.
        ls_property_descriptor-name = ls_hash-key.
        ls_property_descriptor-type = CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_data( lr_hash_data->* ) ).

        APPEND ls_property_descriptor TO lt_components.
      ENDLOOP.

      DATA(lo_merged_type_descriptor) = cl_abap_structdescr=>create( lt_components ).

      DATA lr_merged_data TYPE REF TO data.
      CREATE DATA lr_merged_data TYPE HANDLE lo_merged_type_descriptor.

      " Fill with original data.
      LOOP AT lt_original_properties INTO DATA(lv_original_property).
        ASSIGN COMPONENT lv_original_property OF STRUCTURE lr_data->* TO FIELD-SYMBOL(<from_field>).
        ASSIGN COMPONENT lv_original_property OF STRUCTURE lr_merged_data->* TO FIELD-SYMBOL(<to_field>).

        <to_field> = <from_field>.
      ENDLOOP.

      " Fill with hash data.
      LOOP AT lt_hashes INTO ls_hash.
        ASSIGN COMPONENT lv_key OF STRUCTURE lr_merged_data->* TO FIELD-SYMBOL(<field>).

        <field> = ls_hash_expr_result-data->*.
      ENDLOOP.

      lr_data = lr_merged_data.
      me->backend_push_pseudo_block( lr_merged_data ).
    ENDIF.

    DATA(lr_found_partial) = lr_find_partial_result-partial.
    DATA(ls_partial_result) = lr_found_partial->partial->template_internal(
      ir_data        = lr_data
      it_block_stack = me->mt_backend_block_stack
    ).

    " Pop context "block" if necessary.
    IF lv_new_context_created = abap_true.
      me->backend_pop_block( ).
    ENDIF.

    IF ls_partial_result-error IS NOT INITIAL.
      rs_result-error = ls_partial_result-error.
      RETURN.
    ENDIF.

    rs_result-text = ls_partial_result-text.
  ENDMETHOD.


  METHOD backend_eval_helper.
    DATA: lt_args  TYPE tt_data,
          lv_error TYPE string.

    " "Downcast" to common base.
    DATA(lr_helper) = NEW ts_parser_helper( ).
    ASSIGN ir_helper->* TO FIELD-SYMBOL(<helper_base>).
    MOVE-CORRESPONDING <helper_base> TO lr_helper->*.

    IF lr_helper->name IS INITIAL.
      rs_result-error = 'Invalid helper cast'.
      RETURN.
    ENDIF.

    DATA ls_result TYPE ts_backend_eval_expr_result.

    " Evaluate arguments.
    LOOP AT lr_helper->args INTO DATA(ls_arg).
      ls_result = me->backend_eval_expr(
        ir_stmt = ls_arg
        ir_data = ir_data
      ).
      lv_error = ls_result-error.

      IF lv_error IS NOT INITIAL.
        rs_result-error = lv_error.
        RETURN.
      ENDIF.

      APPEND ls_result-data TO lt_args.
    ENDLOOP.

    DATA lt_hashes TYPE tt_hashes.

    " Evaluate hashes.
    LOOP AT lr_helper->hashes INTO DATA(ls_hash).
      ls_result = me->backend_eval_expr(
        ir_stmt = ls_hash-expression
        ir_data = ir_data
      ).
      lv_error = ls_result-error.

      IF lv_error IS NOT INITIAL.
        rs_result-error = lv_error.
        RETURN.
      ENDIF.

      DATA ls_evaluated_hash TYPE ts_hash.
      CLEAR ls_evaluated_hash.

      ls_evaluated_hash-key = ls_hash-key.
      ls_evaluated_hash-data = ls_result-data.

      APPEND ls_evaluated_hash TO lt_hashes.
    ENDLOOP.

    " Find out if it's a block- or an inline-helper.
    DATA lr_block TYPE REF TO ts_parser_block.

    TRY.
        lr_block ?= ir_helper.
      CATCH cx_root.
        " Nothing to do.
    ENDTRY.

    DATA(lv_is_block) = COND abap_bool(
      WHEN lr_block IS BOUND THEN abap_true
      ELSE abap_false
    ).

    " Push current block to stack for context information...
    IF lv_is_block = abap_true.
      me->backend_push_block( VALUE #( block = lr_block ) ).
    ELSE.
      " ...or set current inline helper values.
      ASSIGN ir_helper->* TO FIELD-SYMBOL(<helper>).
      MOVE-CORRESPONDING <helper> TO me->mv_backend_inline_helper.
    ENDIF.

    rs_result = me->backend_call_helper(
      iv_name   = lr_helper->name
      it_args   = lt_args
      it_hashes = lt_hashes
      ir_data   = ir_data
    ).

    " Pop last entry from block stack.
    IF lv_is_block = abap_true.
      me->backend_pop_block( ).
    ELSE.
      CLEAR me->mv_backend_inline_helper.
    ENDIF.
  ENDMETHOD.


  METHOD backend_eval_block.
    rs_result = me->backend_eval_helper(
      ir_helper = ir_block
      ir_data   = ir_data
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
      DATA(lv_kind) = me->backend_get_data_kind( lr_data ).

      IF lv_kind <> e_backend_data_kind_table.
        lt_data = VALUE #( ( lr_data ) ).
      ELSE.
        ASSIGN lr_data->* TO FIELD-SYMBOL(<table>).
        lt_data = <table>.
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

      READ TABLE lt_data INTO ls_data INDEX 1.

      " Clear passed data if index 1 doesn't exist.
      IF sy-subrc <> 0.
        CLEAR ls_data.
      ENDIF.

      rs_result = me->backend_eval_body(
        ir_block = ls_body
        ir_data  = ls_data
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

    DATA(lv_condition_is_true) = ls_truthy_result-truthy.
    DATA(lv_name) = is_options-name.
    DATA(lr_data) = is_options-data.

    " If unless, reverse the condition result.
    IF lv_name = c_unless.
      lv_condition_is_true = COND abap_bool(
        WHEN lv_condition_is_true = abap_false THEN abap_true
        ELSE abap_false
      ).
    ENDIF.

    IF lv_condition_is_true = abap_true.
      rs_result = me->fn( lr_data ).
    ELSE.
      rs_result = me->inverse( lr_data ).
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
      DATA(lv_text) = VALUE string( ).

      CASE lv_type.
        WHEN e_backend_data_kind_struct.
          DATA(lo_struct_desc) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data_ref( lr_iterable ) ).
          DATA(lt_components) = lo_struct_desc->get_components( ).

          IF lines( lt_components ) > 0.
            LOOP AT lt_components INTO DATA(ls_field).
              DATA(lv_field_name) = ls_field-name.

              ASSIGN lr_iterable->* TO FIELD-SYMBOL(<structure>).
              ASSIGN COMPONENT lv_field_name OF STRUCTURE <structure> TO FIELD-SYMBOL(<field>).

              DATA lr_field TYPE REF TO data.

              GET REFERENCE OF <field> INTO lr_field.
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
      rs_result = me->inverse( is_options-data ).
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
      ir_data   = ir_data
    ).
  ENDMETHOD.


  METHOD backend_eval_log_helper.
    DATA lv_log_text TYPE string.

    LOOP AT it_args INTO DATA(ls_arg).
      IF lv_log_text <> ' '.
        lv_log_text = |{ lv_log_text } |.
      ENDIF.

      ASSIGN ls_arg->* TO FIELD-SYMBOL(<arg>).
      lv_log_text = |{ lv_log_text }{ <arg> }|.
    ENDLOOP.

    IF lv_log_text <> ' '.
      WRITE / lv_log_text.
    ENDIF.
  ENDMETHOD.


  METHOD backend_eval_sub_expr.
    rs_result = me->backend_eval_expr(
      ir_stmt = ir_sub_expr->expr
      ir_data = ir_data
    ).
  ENDMETHOD.


  METHOD backend_eval_path.
    DATA: lr_this        TYPE REF TO data,
          lr_block       TYPE REF TO ts_backend_block_stack_block,
          lv_block_index TYPE i.

    DATA(lt_parts) = ir_path->parts.
    DATA(lv_relative_path_found) = abap_false.
    DATA(lv_undefined) = abap_false.
    DATA(lv_index) = 1.
    DATA(lv_original_block_index) = lines( me->mt_backend_block_stack ).

    " Go back the amount of relative steps.
    WHILE lv_index <= lines( lt_parts ).
      IF lt_parts[ 1 ] = c_relative.
        lv_block_index = lv_original_block_index - lv_index.

        me->backend_get_block(
          EXPORTING
            iv_index    = lv_block_index
          IMPORTING
            er_block    = lr_block
            ev_fallback = DATA(lv_fallback)
        ).
        DELETE lt_parts INDEX 1.

        " Fallback block certainly has data.
        IF lv_fallback = abap_true.
          lr_this = lr_block->args[ 1 ]-data.
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
      lr_this = ir_data.
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
            backend_get_block(
              EXPORTING
                iv_index    = lv_block_index
              IMPORTING
                er_block    = lr_block
                ev_fallback = lv_fallback
            ).

            IF lv_fallback = abap_true.
              EXIT.
            ENDIF.

            DATA ls_arg TYPE ts_backend_block_arg.
            READ TABLE lr_block->args INTO ls_arg WITH KEY param-name = lv_part.

            IF sy-subrc = 0.
              lr_this ?= ls_arg-data.

              " Try to evaluate if found data is a literal.
              DATA(ls_literal_result) = me->backend_eval_literal_expr( ir_stmt = lr_this ).

              lv_only_down = abap_true.

              " If it is a literal, evaluate its value.
              IF ls_literal_result-error IS INITIAL.
                ASSIGN lr_this->* TO FIELD-SYMBOL(<literal_structure>).
                ASSIGN COMPONENT 'value' OF STRUCTURE <literal_structure> TO FIELD-SYMBOL(<value>).

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
              ASSIGN lr_this->* TO FIELD-SYMBOL(<structure>).
              ASSIGN COMPONENT lv_part OF STRUCTURE <structure> TO FIELD-SYMBOL(<field>).

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
                me->backend_get_block(
                  EXPORTING
                    iv_index    = lv_block_index
                  IMPORTING
                    er_block    = lr_block
                    ev_fallback = lv_fallback
                ).

                IF lv_fallback = abap_true.
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
            ir_data = ir_data
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


  METHOD backend_push_block.
    APPEND is_block TO me->mt_backend_block_stack.
  ENDMETHOD.


  METHOD backend_push_pseudo_block.
    me->backend_push_block( VALUE #(
      pseudo = abap_true
      args   = VALUE #(
        ( data = ir_data )
      )
    ) ).
  ENDMETHOD.


  METHOD backend_pop_block.
    DATA(lr_block) = me->backend_get_last_block( ).

    rs_block = lr_block->*.
    DELETE me->mt_backend_block_stack INDEX lines( me->mt_backend_block_stack ).
  ENDMETHOD.


  METHOD backend_get_block.
    CLEAR er_block.
    CLEAR ev_fallback.

    " If index is smaller than 1, it's set to one. This way it's ensured
    " that at least the root data (pseudo block) is returned.
    IF iv_index < 1.
      iv_index = 1.
      ev_fallback = abap_true.
    ENDIF.

    READ TABLE me->mt_backend_block_stack REFERENCE INTO er_block INDEX iv_index.

    IF sy-subrc <> 0.
      FREE er_block.
    ENDIF.
  ENDMETHOD.


  METHOD backend_get_last_block.
    me->backend_get_block(
      EXPORTING
        iv_index = lines( mt_backend_block_stack )
      IMPORTING
        er_block = rr_block
        ev_fallback = DATA(lv_data)
    ).
  ENDMETHOD.


  METHOD is_truthy.
    DATA(lv_kind) = me->backend_get_data_kind( ir_data ).
    DATA(lv_truthy) = abap_false.

    CASE lv_kind.
      WHEN e_backend_data_kind_simple.
        ASSIGN ir_data->* TO FIELD-SYMBOL(<bool>).
        lv_truthy = COND abap_bool(
          WHEN <bool> <> ' ' THEN abap_true
          ELSE abap_false
        ).

      WHEN e_backend_data_kind_struct.
        DATA(lo_struct_desc) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data_ref( ir_data ) ).
        DATA(lt_components) = lo_struct_desc->get_components( ).

        lv_truthy = COND abap_bool(
          WHEN lines( lt_components ) > 0 THEN abap_true
          ELSE abap_false
        ).

      WHEN e_backend_data_kind_table.
        FIELD-SYMBOLS: <table> TYPE ANY TABLE.

        ASSIGN ir_data->* TO <table>.
        lv_truthy = COND abap_bool(
          WHEN lines( <table> ) > 0 THEN abap_true
          ELSE abap_false
        ).

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
        DATA ls_options TYPE ts_options.

        ls_options-name     = iv_name.
        ls_options-instance = me.
        ls_options-args     = it_args.
        ls_options-hashes   = it_hashes.
        ls_options-data     = ir_data.

        DATA(lv_type_name) = ls_type-name.

        CASE lv_type_name.
          WHEN 'ts_class_helper'.
            DATA lr_class_helper_config TYPE REF TO ts_class_helper.
            lr_class_helper_config ?= lr_helper.

            DATA(lv_class_name) = lr_class_helper_config->class_name.
            DATA(lv_class_method_name) = lr_class_helper_config->method_name.

            TRANSLATE lv_class_name TO UPPER CASE.
            TRANSLATE lv_class_method_name TO UPPER CASE.

            CALL METHOD (lv_class_name)=>(lv_class_method_name)
              EXPORTING
                it_args    = it_args
                is_options = ls_options
              RECEIVING
                rs_result  = rs_result.

          WHEN 'ts_object_helper'.
            DATA lr_object_helper_config TYPE REF TO ts_object_helper.
            lr_object_helper_config ?= lr_helper.

            DATA(lv_object_method_name) = lr_object_helper_config->method_name.
            TRANSLATE lv_object_method_name TO UPPER CASE.

            CALL METHOD lr_object_helper_config->object->(lv_object_method_name)
              EXPORTING
                it_args    = it_args
                is_options = ls_options
              RECEIVING
                rs_result  = rs_result.

          WHEN 'ts_func_module_helper'.
            DATA lr_func_module_helper_config TYPE REF TO ts_func_module_helper.
            lr_func_module_helper_config ?= lr_helper.

            DATA(lv_function_name) = lr_func_module_helper_config->function_name.
            TRANSLATE lv_function_name TO UPPER CASE.

            CALL FUNCTION lr_func_module_helper_config->function_name
              EXPORTING
                it_args    = it_args
                is_options = ls_options
              IMPORTING
                es_result  = rs_result.

          WHEN 'ts_form_helper'.
            DATA lr_form_helper_config TYPE REF TO ts_form_helper.
            lr_form_helper_config ?= lr_helper.

            DATA(lv_form_name) = lr_form_helper_config->form_name.
            DATA(lv_report_name) = lr_form_helper_config->report_name.
            DATA(lt_args) = it_args.

            TRANSLATE lv_form_name TO UPPER CASE.
            TRANSLATE lv_report_name TO UPPER CASE.

            PERFORM (lv_form_name) IN PROGRAM (lv_report_name)
              USING
                lt_args
                ls_options
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

        ASSIGN ir_struct->* TO FIELD-SYMBOL(<token_base>).
        MOVE-CORRESPONDING <token_base> TO ls_token_base.

        IF sy-subrc = 0.
          rs_token = ls_token_base-token.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
