# HandlebarsABAP
[Handlebars](https://handlebarsjs.com/) implementation for ABAP. For details about the templating language itself, please refer to the original [documentation](https://handlebarsjs.com/guide/).

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
  '{{#with this.title as |title|}}' &
    '{{#if title.front}}' &
      '{{title.front}} ' &
    '{{/if}}' &
  '{{/with}}' &

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

## What's supported so far
### Fields
```hbs
{{firstName}} {{lastName}}
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
It's possible to write and configure custom helpers like in HandlebarsJS but it works a bit differently. In HandlebarsJS a *fn*- and a *reverse*-function is passed to the helper which then need to be called to render either the block-content or the else-content. Since ABAP does not allow to pass functions to other functions or methods, the corresponding functions are part of the *zbc_handlebars_abap* class. The instance of the class is passed to the helper and the helper then needs to either invoke *fn* or *reverse* on that instance.

```abap
METHOD hello.
  DATA lv_name TYPE string.
  lv_name = it_args[ 1 ]-this->*.

  rs_result = io_instance->fn( it_data = VALUE zcl_handlebars_abap=>tt_data(
    ( this = NEW string( |Hello { lv_name } | ) )
  ) ).
ENDMETHOD.
```

#### How to handle data
Passing data to the *fn*/*reverse* method unfortunately is not quite user-friendly yet, this might improve in future releases. Here's an attempt to explain the structure anyways.

```abap
TYPES: BEGIN OF ts_data,
         this   TYPE REF TO data,
         parent TYPE REF TO data, " ts_data
       END OF ts_data.

TYPES: tt_data TYPE STANDARD TABLE OF ts_data WITH DEFAULT KEY.
```

*ts_data* holds two fields:
- *this*: Refers to the actual data. This could be a structure, a table or something simple like string, int, bool, ...
- *parent*: Refers to the data's parent. This must be a reference to a *ts_data* struct.

Those two fields are required internally to traverse upwards if a field is not found in the currently processed structure or table. When *fn* or *reverse* gets called, the caller decides what's considered the parent for the passed data. E.g. the *each*-helper also passes the current index/key to the block it's wrapping. The index/key however has no parent so *parent* stays uninitialized. To get a feeling how to implement helpers it can be useful to have a look into *backend_eval_cond_helper*/*backend_eval_each_helper*/*backend_eval_with_helper*.

#### Custom-helper helper-methods
- *is_truthy*: Returns *abap_true* if the passed data evaluates to something that is considered truthy by HandlebarsJS standars.
- *error*: Creates an error string that contains the position where the error occurred.

#### How to register a custom helper
To register a custom helper there are two possible ways. Either globally, directly on the *zcl_handlebars_abap* class or on an instance which gets returned calling *compile*. Globally configured helpers get added to the instances created. However, helpers registered on the instances take precedence.

```abap
register_helper( iv_name = 'hello' ir_helper = NEW ts_object_helper( object = lo_helper_object method_name = 'hello_helper' ) ).
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
    is_data          TYPE zcl_handlebars_abap=>ts_data
  RETURNING
    VALUE(rs_result) TYPE zcl_handlebars_abap=>ts_text_result.
```

To register a class-method, use the *ts_class_helper* structure. E.g.

```abap
register_helper( iv_name = 'hello' ir_helper = NEW ts_class_helper( class_name = 'ZCL_HELPER_CLASS' method_name = 'hello_helper' ) ).
```

To register an object-method, use the *ts_object_helper* structure. E.g.

```abap
register_helper( iv_name = 'hello' ir_helper = NEW ts_object_helper( object = lo_helper_object method_name = 'hello_helper' ) ).
```

#### Function modules
A function module must implement the following signature to be callable.

```abap
FUNCTION helper_method
  IMPORTING
    io_instance TYPE zcl_handlebars_abap
    iv_name     TYPE string
    it_args     TYPE zcl_handlebars_abap=>tt_data
    is_data     TYPE zcl_handlebars_abap=>ts_data
  EXPORTING
    es_result   TYPE zcl_handlebars_abap=>ts_text_result.
```

To register a function module, use the *ts_func_module_helper* structure. E.g.

```abap
register_helper( iv_name = 'hello' ir_helper = NEW ts_func_module_helper( function_name = 'ZCL_HELLO_HELPER' ) ).
```

#### Forms
A form must implement the following signature to be callable.

```abap
FORM helper_method
  USING
    io_instance TYPE zcl_handlebars_abap
    iv_name     TYPE string
    it_args     TYPE zcl_handlebars_abap=>tt_data
    is_data     TYPE zcl_handlebars_abap=>ts_data
  CHANGING
    cs_result   TYPE zcl_handlebars_abap=>ts_text_result.
```
