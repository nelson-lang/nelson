%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% svg = NFlow.internal.acausalGenerateIcon(name)
%
% Acausal palette N5: the per-block icon generator. Emits a self-contained
% SVG palette icon for one acausal component, in the layout of the
% OpenModelica documentation icons (one unit per pixel, viewBox = bounding
% box of the graphics, about 306 px wide). The geometry is the icon of the
% equivalent Modelica Standard Library class (extracted by
% docs/tools/extract_msl_icons.py into libraries/msl-icons.json; BSD 3-Clause,
% see THIRDPARTY.md); only the colours are the nflow domain palette: a card
% tinted by physical domain, strokes and texts in the domain ink, fills
% mapped to white / tint / ink (NFlow.internal.mslIconSvg).
%
% Deterministic and generated from the single-source catalog, so every block
% ships with a consistent icon; the GUI palette (N5) references these under
% <libraries>/acausal_<domain>/exports/<name>.svg.
%
% name: a component name from NFlow.internal.acausalCatalog (e.g. 'Resistor').
%=============================================================================
function svg = acausalGenerateIcon(name)
  catalog = NFlow.internal.acausalCatalog();
  idx = 0;
  for k = 1:numel(catalog)
    if strcmp(catalog(k).name, name)
      idx = k; break
    end
  end
  if idx == 0
    error('Nelson:nflow:acausalIconUnknown', ...
      'Unknown acausal component ''%s''.', name);
  end
  data = ic_data();
  block = [];
  for k = 1:numel(data.blocks)
    b = ic_item(data.blocks, k);
    if strcmp(b.name, name)
      block = b; break
    end
  end
  if isempty(block)
    error('Nelson:nflow:acausalIconNoData', ...
      'No Modelica icon data for acausal component ''%s''.', name);
  end
  [bg, ink, tint] = ic_palette(catalog(idx).domain);
  svg = NFlow.internal.mslIconSvg(block, struct('bg', bg, 'ink', ink, 'tint', tint), name);
end
%=============================================================================
function data = ic_data()
  p = [modulepath('nflow_blocks', 'root'), '/libraries/msl-icons.json'];
  data = jsondecode(fileread(p));
end
%=============================================================================
function it = ic_item(list, k)
  if iscell(list)
    it = list{k};
  else
    it = list(k);
  end
end
%=============================================================================
function [bg, ink, tint] = ic_palette(domain)
  switch domain
    case 'electrical'
      bg = '#E8F0FE'; ink = '#1A3E8C'; tint = '#9DB8F0';
    case 'thermal'
      bg = '#FDECEA'; ink = '#9C2A1A'; tint = '#F0A99D';
    case 'translational'
      bg = '#FEF6E1'; ink = '#8C5A08'; tint = '#E7C98A';
    case 'rotational'
      bg = '#EAF7EC'; ink = '#1E6B2E'; tint = '#A6DDB0';
    otherwise
      bg = '#F0F0F2'; ink = '#333333'; tint = '#BEBEC2';
  end
end
%=============================================================================
