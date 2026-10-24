%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% A representative bespoke symbol (not the labelled fallback) is drawn for the
% core electrical / mechanical components.
for nm = {'Resistor', 'Capacitor', 'Inductor', 'Diode', 'Mass', 'Spring', 'Damper', 'IdealGear'}
  svg = NFlow.internal.acausalGenerateIcon(nm{1});
  assert_istrue(~isempty(strfind(svg, '<rect ')) || ~isempty(strfind(svg, '<polyline ')));
  assert_istrue(~isempty(strfind(svg, ['>', nm{1}, '</text>'])));
end
%=============================================================================
% Unknown component errors with the documented identifier.
msg = '';
try
  NFlow.internal.acausalGenerateIcon('NoSuchComponent');
catch e
  msg = e.identifier;
end
assert_istrue(strcmp(msg, 'Nelson:nflow:acausalIconUnknown'));
%=============================================================================
