# HandlebarsABAP
[Handlebars](https://handlebarsjs.com/) implementation for ABAP. For details about the templating language itself, please refer to the original [documentation](https://handlebarsjs.com/guide/).

```abap
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
DATA(lr_person) = NEW ts_person(
  title = VALUE #( front = 'Ing.' back = 'BSc.' ) firstName = 'Peter' lastName = 'Parker'
).

" Template.
DATA(ls_template_result) = ls_compile_result-template->template( lr_person ).

WRITE / ls_template_result-text. " Prints 'Ing. Peter Parker'.
```
