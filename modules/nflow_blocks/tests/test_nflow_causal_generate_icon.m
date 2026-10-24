%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% NFlow.internal.causalGenerateIcon renders the Modelica icon of every causal
% block that has one (libraries/msl-icons.json entries carrying a library) in
% the OpenModelica documentation layout, labelled with the manifest label.
data = jsondecode(fileread([modulepath('nflow_blocks', 'root'), '/libraries/msl-icons.json']));
n = 0;
for k = 1:numel(data.blocks)
  if iscell(data.blocks)
    b = data.blocks{k};
  else
    b = data.blocks(k);
  end
  if ~isfield(b, 'lib') || isempty(b.lib)
    continue
  end
  svg = NFlow.internal.causalGenerateIcon(b.lib, b.name);
  assert_istrue(~isempty(strfind(svg, 'viewBox="')));
  assert_istrue(~isempty(strfind(svg, 'transform="matrix(1 0 0 -1 0 0)"')));
  assert_istrue(~isempty(strfind(svg, '</svg>')));
  n = n + 1;
end
assert_istrue(n >= 60);
% The label of the manifest replaces the Modelica instance name.
svg = NFlow.internal.causalGenerateIcon('continuous', 'tf');
assert_istrue(~isempty(strfind(svg, '>Transfer Fn</text>')));
% The discrete library keeps its dashed card.
svg = NFlow.internal.causalGenerateIcon('discrete', 'zoh');
assert_istrue(~isempty(strfind(svg, 'stroke-dasharray')));
% Unknown block errors with the documented identifier.
msg = '';
try
  NFlow.internal.causalGenerateIcon('continuous', 'noSuchBlock');
catch e
  msg = e.identifier;
end
assert_istrue(strcmp(msg, 'Nelson:nflow:causalIconNoData'));
%=============================================================================
