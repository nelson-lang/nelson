//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { createElement } from 'react';
import { BlockNoteSchema, defaultBlockSpecs } from '@blocknote/core';
import { createReactBlockSpec } from '@blocknote/react';
import { CodeCell } from './blocks/CodeCell';
import { SliderCell } from './blocks/SliderCell';
import { EquationCell } from './blocks/EquationCell';
import {
  NumberFieldCell,
  DropdownCell,
  CheckboxCell,
  ButtonCell,
  StateButtonCell,
  TextFieldCell,
  ColorPickerCell,
  DatePickerCell,
  RangeSliderCell,
  FileBrowserCell,
  VariableSelectorCell,
} from './blocks/ControlBlocks';
import { LiveTaskCell } from './blocks/LiveTaskCell';
//=============================================================================
// NB: the block type must not be `code` — BlockNote has a built-in inline `code`
// mark, and a block of the same name throws "code can not be both a node and a
// mark". createReactBlockSpec returns a factory in 0.55, invoked below.
const codeBlock = createReactBlockSpec(
  {
    type: 'codecell',
    propSchema: {
      code: { default: '' },
      output: { default: '' },
      figures: { default: '[]' },
      // Parallel to `figures`: the live interactive figure handle for each image,
      // or -1 for a non-interactive/restored image. Session-scoped (not exported).
      figureHandles: { default: '[]' },
      // Monotonic session revision used to refresh an inline figure even when a
      // rerun produces the same fallback image URL.
      runRevision: { default: 0 },
      errorMsg: { default: '' },
      stale: { default: false },
    },
    content: 'none',
  },
  { render: (props) => createElement(CodeCell, { block: props.block, editor: props.editor }) }
);
//=============================================================================
const equationBlock = createReactBlockSpec(
  { type: 'equation', propSchema: { latex: { default: 'y = f(x)' } }, content: 'none' },
  { render: (props) => createElement(EquationCell, { block: props.block, editor: props.editor }) }
);
//=============================================================================
const sliderBlock = createReactBlockSpec(
  {
    type: 'slider',
    propSchema: {
      label: { default: 'Parameter' },
      varName: { default: 'a' },
      value: { default: 1 },
      min: { default: 0 },
      max: { default: 10 },
      step: { default: 0.1 },
      runMode: { default: 'all' },
      runOn: { default: 'change' },
    },
    content: 'none',
  },
  { render: (props) => createElement(SliderCell, { block: props.block, editor: props.editor }) }
);
//=============================================================================
const numberBlock = createReactBlockSpec(
  {
    type: 'numberfield',
    propSchema: {
      label: { default: 'Value' },
      varName: { default: 'n' },
      value: { default: 1 },
      min: { default: 0 },
      max: { default: 100 },
      step: { default: 1 },
      runMode: { default: 'all' },
    },
    content: 'none',
  },
  {
    render: (props) => createElement(NumberFieldCell, { block: props.block, editor: props.editor }),
  }
);
//=============================================================================
const dropdownBlock = createReactBlockSpec(
  {
    type: 'dropdown',
    propSchema: {
      label: { default: 'Choice' },
      varName: { default: 'k' },
      value: { default: '1' },
      options: { default: '1, 2, 5, 10' },
      labels: { default: '' },
      optionsVar: { default: '' },
      runMode: { default: 'all' },
    },
    content: 'none',
  },
  { render: (props) => createElement(DropdownCell, { block: props.block, editor: props.editor }) }
);
//=============================================================================
const checkboxBlock = createReactBlockSpec(
  {
    type: 'checkbox',
    propSchema: {
      label: { default: 'Enable' },
      varName: { default: 'flag' },
      checked: { default: true },
      runMode: { default: 'all' },
    },
    content: 'none',
  },
  { render: (props) => createElement(CheckboxCell, { block: props.block, editor: props.editor }) }
);
//=============================================================================
const buttonBlock = createReactBlockSpec(
  { type: 'runbutton', propSchema: { caption: { default: 'Run' } }, content: 'none' },
  { render: (props) => createElement(ButtonCell, { block: props.block, editor: props.editor }) }
);
//=============================================================================
const stateButtonBlock = createReactBlockSpec(
  {
    type: 'statebutton',
    propSchema: {
      label: { default: 'Toggle' },
      varName: { default: 's' },
      on: { default: false },
      runMode: { default: 'all' },
    },
    content: 'none',
  },
  {
    render: (props) => createElement(StateButtonCell, { block: props.block, editor: props.editor }),
  }
);
//=============================================================================
const textBlock = createReactBlockSpec(
  {
    type: 'textfield',
    propSchema: {
      label: { default: 'Text' },
      varName: { default: 't' },
      value: { default: '0' },
      runMode: { default: 'all' },
    },
    content: 'none',
  },
  { render: (props) => createElement(TextFieldCell, { block: props.block, editor: props.editor }) }
);
//=============================================================================
const colorBlock = createReactBlockSpec(
  {
    type: 'colorpicker',
    propSchema: {
      label: { default: 'Color' },
      varName: { default: 'c' },
      value: { default: '#0072bd' },
      runMode: { default: 'all' },
    },
    content: 'none',
  },
  {
    render: (props) => createElement(ColorPickerCell, { block: props.block, editor: props.editor }),
  }
);
//=============================================================================
const dateBlock = createReactBlockSpec(
  {
    type: 'datepicker',
    propSchema: {
      label: { default: 'Date' },
      varName: { default: 'day' },
      value: { default: '2026-06-15' },
      runMode: { default: 'all' },
    },
    content: 'none',
  },
  { render: (props) => createElement(DatePickerCell, { block: props.block, editor: props.editor }) }
);
//=============================================================================
const rangeBlock = createReactBlockSpec(
  {
    type: 'rangeslider',
    propSchema: {
      label: { default: 'Range' },
      varName: { default: 'r' },
      low: { default: 2 },
      high: { default: 8 },
      min: { default: 0 },
      max: { default: 10 },
      step: { default: 0.1 },
      runMode: { default: 'all' },
      runOn: { default: 'change' },
    },
    content: 'none',
  },
  {
    render: (props) => createElement(RangeSliderCell, { block: props.block, editor: props.editor }),
  }
);
//=============================================================================
const fileBlock = createReactBlockSpec(
  {
    type: 'filebrowser',
    propSchema: {
      label: { default: 'File' },
      varName: { default: 'f' },
      filename: { default: '' },
      value: { default: '' },
      runMode: { default: 'all' },
    },
    content: 'none',
  },
  {
    render: (props) => createElement(FileBrowserCell, { block: props.block, editor: props.editor }),
  }
);
//=============================================================================
const varSelectBlock = createReactBlockSpec(
  {
    type: 'varselect',
    propSchema: {
      label: { default: 'Variable' },
      varName: { default: 'v' },
      value: { default: '' },
      runMode: { default: 'all' },
    },
    content: 'none',
  },
  {
    render: (props) =>
      createElement(VariableSelectorCell, { block: props.block, editor: props.editor }),
  }
);
//=============================================================================
const liveTaskBlock = createReactBlockSpec(
  {
    type: 'livetask',
    propSchema: {
      op: { default: 'plot' },
      inputVar: { default: '' },
      outVar: { default: 'result' },
      template: { default: 'result = f($var)' },
      output: { default: '' },
      figures: { default: '[]' },
      errorMsg: { default: '' },
    },
    content: 'none',
  },
  { render: (props) => createElement(LiveTaskCell, { block: props.block, editor: props.editor }) }
);
//=============================================================================
export const schema = BlockNoteSchema.create({
  blockSpecs: {
    ...defaultBlockSpecs,
    codecell: codeBlock(),
    varselect: varSelectBlock(),
    livetask: liveTaskBlock(),
    slider: sliderBlock(),
    numberfield: numberBlock(),
    dropdown: dropdownBlock(),
    checkbox: checkboxBlock(),
    runbutton: buttonBlock(),
    statebutton: stateButtonBlock(),
    textfield: textBlock(),
    colorpicker: colorBlock(),
    datepicker: dateBlock(),
    rangeslider: rangeBlock(),
    filebrowser: fileBlock(),
    equation: equationBlock(),
  },
});
//=============================================================================
export const CONTROL_TYPES = [
  'slider',
  'rangeslider',
  'numberfield',
  'dropdown',
  'checkbox',
  'textfield',
  'colorpicker',
  'datepicker',
  'filebrowser',
  'varselect',
  'runbutton',
  'statebutton',
];
//=============================================================================
export function defaultBlockFor(type: string): any {
  switch (type) {
    case 'codecell':
      return { type: 'codecell', props: { code: 'y = 2 + 2' } };
    case 'equation':
      return { type: 'equation', props: { latex: 'E = mc^2' } };
    case 'slider':
      return {
        type: 'slider',
        props: { label: 'Parameter', varName: 'a', value: 1, min: 0, max: 10, step: 0.1 },
      };
    case 'rangeslider':
      return {
        type: 'rangeslider',
        props: { label: 'Range', varName: 'r', low: 2, high: 8, min: 0, max: 10, step: 0.1 },
      };
    case 'numberfield':
      return {
        type: 'numberfield',
        props: { label: 'Value', varName: 'n', value: 1, min: 0, max: 100, step: 1 },
      };
    case 'dropdown':
      return {
        type: 'dropdown',
        props: { label: 'Choice', varName: 'k', value: '1', options: '1, 2, 5, 10' },
      };
    case 'checkbox':
      return { type: 'checkbox', props: { label: 'Enable', varName: 'flag', checked: true } };
    case 'textfield':
      return { type: 'textfield', props: { label: 'Text', varName: 't', value: '0' } };
    case 'colorpicker':
      return { type: 'colorpicker', props: { label: 'Color', varName: 'c', value: '#0072bd' } };
    case 'datepicker':
      return { type: 'datepicker', props: { label: 'Date', varName: 'day', value: '2026-06-15' } };
    case 'filebrowser':
      return {
        type: 'filebrowser',
        props: { label: 'File', varName: 'f', filename: '', value: '' },
      };
    case 'statebutton':
      return { type: 'statebutton', props: { label: 'Toggle', varName: 's', on: false } };
    case 'varselect':
      return { type: 'varselect', props: { label: 'Variable', varName: 'v', value: '' } };
    case 'livetask':
      return { type: 'livetask', props: { op: 'plot', inputVar: '', outVar: 'result' } };
    case 'runbutton':
      return { type: 'runbutton', props: { caption: 'Run' } };
    default:
      return { type: 'paragraph', content: '' };
  }
}
//=============================================================================
