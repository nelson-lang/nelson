%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% svg = NFlow.internal.causalGenerateIcon(lib, type)
%
% Palette icon of a causal block that has a Modelica.Blocks equivalent: the
% icon of the Modelica Standard Library class (extracted by
% docs/tools/extract_msl_icons.py into libraries/msl-icons.json; BSD 3-Clause,
% see THIRDPARTY.md), laid out like the OpenModelica documentation icons and
% recoloured with the nflow palette of the block's library
% (NFlow.internal.mslIconSvg). The Modelica name label shows the block label
% of the library manifest.
%
% lib:  builtin library directory name (e.g. 'continuous')
% type: block type in that library (e.g. 'integrator')
%=============================================================================
function svg = causalGenerateIcon(lib, type)
  data = jsondecode(fileread([modulepath('nflow_blocks', 'root'), '/libraries/msl-icons.json']));
  block = [];
  for k = 1:numel(data.blocks)
    b = ci_item(data.blocks, k);
    if isfield(b, 'lib') && strcmp(b.lib, lib) && strcmp(b.name, type)
      block = b; break
    end
  end
  if isempty(block)
    error('Nelson:nflow:causalIconNoData', ...
      'No Modelica icon data for block ''%s/%s''.', lib, type);
  end
  svg = NFlow.internal.mslIconSvg(block, ci_palette(lib), ci_label(lib, type));
end
%=============================================================================
function it = ci_item(list, k)
  if iscell(list)
    it = list{k};
  else
    it = list(k);
  end
end
%=============================================================================
function label = ci_label(lib, type)
  label = type;
  p = [modulepath('nflow_blocks', 'root'), '/libraries/', lib, '/library.json'];
  if ~isfile(p)
    return
  end
  manifest = jsondecode(fileread(p));
  for k = 1:numel(manifest.blocks)
    b = ci_item(manifest.blocks, k);
    if strcmp(b.type, type) && isfield(b, 'label') && ~isempty(b.label)
      label = b.label; return
    end
  end
end
%=============================================================================
function pal = ci_palette(lib)
  % The card colours of each builtin causal library (as its hand-drawn icons).
  switch lib
    case 'continuous'
      pal = struct('bg', '#E1F5EE', 'ink', '#085041', 'tint', '#9FE1CB');
    case 'discrete'
      pal = struct('bg', '#E6F1FB', 'ink', '#0C447C', 'tint', '#B5D4F4', 'dash', true);
    case 'sink'
      pal = struct('bg', '#E6F1FB', 'ink', '#0C447C', 'tint', '#B5D4F4');
    case 'source'
      pal = struct('bg', '#FAEEDA', 'ink', '#633806', 'tint', '#FAC775');
    case 'nonlinear'
      pal = struct('bg', '#FAECE7', 'ink', '#712B13', 'tint', '#F5C4B3');
    case {'math', 'lookup', 'userdefined'}
      pal = struct('bg', '#EEEDFE', 'ink', '#3C3489', 'tint', '#AFA9EC');
    otherwise % logic, utility
      pal = struct('bg', '#F1EFE8', 'ink', '#444441', 'tint', '#B4B2A9');
  end
end
%=============================================================================
