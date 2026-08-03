# HandlebarsABAP
Single-file 7.40 SP02 [Handlebars](https://handlebarsjs.com/) implementation for ABAP.

## What is Handlebars?
To quote the [Handlebars documentation](https://handlebarsjs.com/guide/):
> Handlebars is a simple templating language.
> It uses a template and an input object to generate HTML or other text formats. Handlebars templates look like regular text with embedded Handlebars expressions.
> ```hbs
> <p>{{firstname}} {{lastname}}</p>
> ```
>A handlebars expression is a ```{{```, some contents, followed by a ```}}```. When the template is executed, these expressions are replaced with values from an input object.

## Why Handlebars for ABAP?
Even though the *WWW_HTML_MERGER* function module exists to load and merge HTML templates with simple data, it is very limited in its use. Handlebars on the other hand allows for building highly flexible templates and automatically resolves deeply nested structures and tables, making it easy to quickly build extremely customizable HTMLs/texts based on its input data.

To compile a Handlebars template, simply pass it to the static *compile*-method and call the returned instance's *template*-method to fill the template with the provided data.

```abap
TYPES: BEGIN OF ts_title,
         front TYPE string,
         back  TYPE string,
       END OF ts_title.

TYPES: BEGIN OF ts_person,
         title     TYPE ts_title,
         firstName TYPE string,
         lastName  TYPE string,
       END OF ts_person.

" Compile the Handlebars template.
DATA(ls_compile_result) = zcl_handlebars_abap=>compile(
  '{{#if title.front}}' &
    '{{title.front}} ' &
  '{{/if}}' &

  '{{firstName}} {{lastName}}'
).

" Create a structure.
DATA(ls_person) = VALUE ts_person(
  title = VALUE #( front = 'Ing.' back = 'BSc.' ) firstName = 'Peter' lastName = 'Parker'
).

" Let the magic happen.
DATA(ls_template_result) = ls_compile_result-instance->template( ls_person ).

WRITE / ls_template_result-text. " Prints 'Ing. Peter Parker'.
```

## Installation
HandlebarsABAP can be installed by using the latest release-tag via [abapGit](https://docs.abapgit.org/user-guide/projects/online/install.html) or by simply creating a *ZCL_HANDLEBARS_ABAP*-class on the client and pasting the content of the latest *ZCL_HANDLEBARS_ABAP.abap* release file into it.

## Stored templates
To compile a template stored via transaction SMW0, just pass the template name to compile. It is recommended to store the HTML-template as binary data in SMW0. Not storing it as binary data could lead to a wrong interpretation of language specific letters (e.g. ä, ö, ü, ß, ...). Even though templates stored as HTML are still supported, templates stored as binaries take precedence and are converted to UTF-8.

## What's supported so far
### Fields
```hbs
{{title.front}} {{firstName}} {{lastName}} {{title.back}}
```

### Block helpers
#### Builtin
##### if
```hbs
{{#if condition}}
  render something
{{else}}
  render something else
{{/if}}
```

##### unless
```hbs
{{#unless condition}}
  render something
{{else}}
  render something else
{{/unless}}
```

##### each
```hbs
{{#each people}}
  {{this.firstName}}
{{else}}
  No person found
{{/each}}
```

##### with
```hbs
{{#with person}}
  {{this.firstName}}
{{else}}
  No person found
{{/with}}
```

### Inline helpers
#### Builtin
##### log
```hbs
{{log "Hello World"}}
```

##### lookup
```hbs
{{lookup (lookup employees 0) "firstname"}}
```

### Block parameters
```hbs
{{#each people as |person index|}}
  {{index}}: {{person.firstName}}
{{else}}
  No person found
{{/each}}
```

### Sub expressions
```hbs
{{#if (log "Hello World")}}
  what?
{{else}}
  log didn't return anything
{{/if}}
```

### Partials
```hbs
{{! name partial}}
{{firstName}} {{lastName}}
```

```hbs
{{! card partial}}
-----------------------------------------------
Name: {{> name .}}
Description: {{description}}
Manager: {{#if is_manager}}Yes{{else}}No{{/if}}
```

```hbs
{{> card this is_manager=true}}

{{#each employees}}
{{> card .}}
{{/each}}
```

#### How to register a partial
Partials can either be registered globally, directly on the *zcl_handlebars_abap* class, using *register_partial_static* or on an instance which gets returned calling *compile*, using *register_partial*. Globally configured partials get added to the instances created. However, partials registered on the instances take precedence.

```abap
register_partial( iv_name = 'name' iv_template_string = '{{firstName}} {{lastName}}' ).
```

### Custom (block) helpers
It's possible to write and configure custom helpers like in HandlebarsJS but it works a bit differently. In HandlebarsJS a *fn*- and a *reverse*-function is passed to the helper which then need to be called to render either the block-content or the else-content. Since ABAP does not allow to pass functions to other functions or methods, the corresponding functions are part of the *zcl_handlebars_abap* class. The instance of the class gets passed to the helper and the helper needs to either invoke *fn* or *reverse* on that instance. The first argument that's passed to the corresponding function is considered the new context within the block (*this*).

```abap
METHOD hello.
  ASSIGN it_args[ 1 ]->* TO FIELD-SYMBOL(<name>).

  rs_result = io_instance->fn( NEW string( |Hello { <name> } | ) ).
ENDMETHOD.
```

#### How to handle data
Each helper function receives an *it_args* table as argument. This table contains references to the data passed, which could be a struct, a table or something simple like bool, int, string... The same holds true for the hash arguments passed via *is_options-hashes*.

It's the implementer's responsibility to handle the data correctly. To get a feeling how to implement helpers it can be useful to have a look into *backend_eval_cond_helper*/*backend_eval_each_helper*/*backend_eval_with_helper*.

#### What to return
The *rs_result*'s *text*-property specifies, what will be rendered to the output. If something went horribly wrong the *rs_result*'s *error*-property must be set. This will halt further template execution and will propergate the error up to the end result.

#### Custom-helper helper-methods
- *is_truthy*: Returns *abap_true* if the passed data evaluates to something that is considered truthy by HandlebarsJS standars.
- *error*: Creates an error string that contains the position where the error occurred.

#### How to register a custom helper
Custom helpers can either be registered globally, directly on the *zcl_handlebars_abap* class, using *register_helper_static* or on an instance which gets returned calling *compile*, using *register_helper*. Globally configured helpers get added to the instances created. However, helpers registered on the instances take precedence.

```abap
register_helper( iv_name = 'hello' ir_helper = NEW zcl_handlebars_abap=>ts_object_helper( object = lo_helper_object method_name = 'hello_helper' ) ).
```

#### Function/Method signatures
Helper functions/methods get passed the following properties **IMPORTANT**: Be aware of the breaking changes introduced in [v1.0.0](https://github.com/monstermichl/HandlebarsABAP/issues/8#issuecomment-5072874511) and [v2.0.0](https://github.com/monstermichl/HandlebarsABAP/issues/23#issuecomment-5158051675).
- args: The arguments passed to the helper.
- options:
  - instance: The instance of the currently processed object on which to call *fn*/*reverse*.
  - name: The name of the registered helper. This can be useful if method gets registered for different helper names.
  - hashes: The hash arguments passed to the helper.
  - data: The current data of the block.

##### (Class-)methods
This is the prefered way to implement helpers as it goes hand in hand with the modern approach of ABAP programming. Both static- and object-methods must implement the following signature to be callable in case of a block helper.

```abap
METHODS block_helper_method
  IMPORTING
    it_args          TYPE zcl_handlebars_abap=>tt_data
    is_options       TYPE zcl_handlebars_abap=>ts_options
  RETURNING
    VALUE(rs_result) TYPE zcl_handlebars_abap=>ts_text_result.
```

In case of an inline helper, the returned struct-type changes from *zcl_handlebars_abap=>ts_text_result* to *zcl_handlebars_abap=>ts_data_result*. This also applies to [Function modules](#function-modules) and [Forms](#forms)!

**IMPORTANT**: Since *ts_data_result* contains a reference to the returned data, it is necessary to put the data on the heap instead of the stack (e.g. by using [CREATE DATA](https://help.sap.com/doc/abapdocu_816_index_htm/8.16/en-US/ABAPCREATE_DATA.html)).

```abap
METHODS inline_helper_method
  IMPORTING
    it_args          TYPE zcl_handlebars_abap=>tt_data
    is_options       TYPE zcl_handlebars_abap=>ts_options
  RETURNING
    VALUE(rs_result) TYPE zcl_handlebars_abap=>ts_data_result.
```

To register a class-method, use the *ts_class_helper* structure. E.g.

```abap
register_helper( iv_name = 'hello' ir_helper = NEW zcl_handlebars_abap=>ts_class_helper( class_name = 'ZCL_HELPER_CLASS' method_name = 'hello_helper' ) ).
```

To register an object-method, use the *ts_object_helper* structure. E.g.

```abap
register_helper( iv_name = 'hello' ir_helper = NEW zcl_handlebars_abap=>ts_object_helper( object = lo_helper_object method_name = 'hello_helper' ) ).
```

#### Function modules
A function module must implement the following signature to be callable.

```abap
FUNCTION helper_method
  IMPORTING
    it_args    TYPE zcl_handlebars_abap=>tt_data
    is_options TYPE zcl_handlebars_abap=>ts_options
  EXPORTING
    es_result  TYPE zcl_handlebars_abap=>ts_text_result.
```

To register a function module, use the *ts_func_module_helper* structure. E.g.

```abap
register_helper( iv_name = 'hello' ir_helper = NEW zcl_handlebars_abap=>ts_func_module_helper( function_name = 'ZCL_HELLO_HELPER' ) ).
```

#### Forms
A form must implement the following signature to be callable.

```abap
FORM helper_method
  USING
    it_args    TYPE zcl_handlebars_abap=>tt_data
    is_options TYPE zcl_handlebars_abap=>ts_options
  CHANGING
    cs_result  TYPE zcl_handlebars_abap=>ts_text_result.
```

To register a form, use the *ts_form_helper* structure. E.g.

```abap
register_helper( iv_name = 'hello' ir_helper = NEW zcl_handlebars_abap=>ts_form_helper( report_name = 'Z_HELLO_HELPER' form_name = 'hello_helper' ) ).
```
