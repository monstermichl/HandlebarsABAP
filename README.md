# HandlebarsABAP
Single-file [Handlebars](https://handlebarsjs.com/) implementation for ABAP. For details about the templating language itself, please refer to the original [documentation](https://handlebarsjs.com/guide/).

To compile a Handlebars template, simply pass it to the static *compile*-method. If a template-name (transaction SMW0) gets passed, the template gets loaded instead.

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
HandlebarsABAP can be installed via [abapGit](https://docs.abapgit.org/user-guide/projects/online/install.html) or by just creating a *ZCL_HANDLEBARS_ABAP*-class on the client and pasting the content of the latest *ZCL_HANDLEBARS_ABAP.abap* release file into it.

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

### Custom helpers
It's possible to write and configure custom helpers like in HandlebarsJS but it works a bit differently. In HandlebarsJS a *fn*- and a *reverse*-function is passed to the helper which then need to be called to render either the block-content or the else-content. Since ABAP does not allow to pass functions to other functions or methods, the corresponding functions are part of the *zbc_handlebars_abap* class. The instance of the class gets passed to the helper and the helper needs to either invoke *fn* or *reverse* on that instance. The first argument that's passed to the corresponding function is considered the new context within the block (*this*).

```abap
METHOD hello.
  DATA lv_name TYPE string.
  lv_name = it_args[ 1 ]->*.

  rs_result = io_instance->fn( NEW string( |Hello { lv_name } | ) ).
ENDMETHOD.
```

#### How to handle data
Each helper function receives an *it_args* table as argument. This table contains references to the data passed, which could be a struct, a table or something simple like bool, int, string...

It's the implementer's responsibility to handle the data correctly. To get a feeling how to implement helpers it can be useful to have a look into *backend_eval_cond_helper*/*backend_eval_each_helper*/*backend_eval_with_helper*.

#### What to return
The *rs_result*'s *text*-property specifies, what will be rendered to the output. If something went horribly wrong the *rs_result*'s *error*-property must be set. This will halt further template execution and will propergate the error up to the end result.

#### Custom-helper helper-methods
- *is_truthy*: Returns *abap_true* if the passed data evaluates to something that is considered truthy by HandlebarsJS standars.
- *error*: Creates an error string that contains the position where the error occurred.

#### How to register a custom helper
To register a custom helper there are two possible ways. Either globally, directly on the *zcl_handlebars_abap* class or on an instance which gets returned calling *compile*. Globally configured helpers get added to the instances created. However, helpers registered on the instances take precedence.

```abap
register_helper( iv_name = 'hello' ir_helper = NEW zcl_handlebars_abap=>ts_object_helper( object = lo_helper_object method_name = 'hello_helper' ) ).
```

#### Function/Method signatures
Helper functions/methods get passed the following properties:
- instance: The instance of the currently processed object on which to call *fn*/*reverse*.
- name: The name of the registered helper. This can be useful if method gets registered for different helper names.
- args: The arguments passed to the helper.
- data: The current data of the block.

##### (Class-)methods
This is the prefered way to implement helpers as it goes hand in hand with the modern approach of ABAP programming. Both static and object methods must implement the following signature to be callable.

```abap
METHODS helper_method
  IMPORTING
    io_instance      TYPE zcl_handlebars_abap
    iv_name          TYPE string
    it_args          TYPE zcl_handlebars_abap=>tt_data
    ir_data          TYPE zcl_handlebars_abap=>tr_data
  RETURNING
    VALUE(rs_result) TYPE zcl_handlebars_abap=>ts_text_result.
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
    io_instance TYPE zcl_handlebars_abap
    iv_name     TYPE string
    it_args     TYPE zcl_handlebars_abap=>tt_data
    ir_data     TYPE zcl_handlebars_abap=>tr_data
  EXPORTING
    es_result   TYPE zcl_handlebars_abap=>ts_text_result.
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
    io_instance TYPE zcl_handlebars_abap
    iv_name     TYPE string
    it_args     TYPE zcl_handlebars_abap=>tt_data
    ir_data     TYPE zcl_handlebars_abap=>tr_data
  CHANGING
    cs_result   TYPE zcl_handlebars_abap=>ts_text_result.
```

To register a form, use the *ts_form_helper* structure. E.g.

```abap
register_helper( iv_name = 'hello' ir_helper = NEW zcl_handlebars_abap=>ts_form_helper( report_name = 'Z_HELLO_HELPER' form_name = 'hello_helper' ) ).
```
