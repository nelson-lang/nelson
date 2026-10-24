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
% Acausal palette N0.2: NFlow.internal.acausalGenerateHelp emits a well-formed Nelson
% help XML page per catalog component (en + fr). The generated XML must parse
% (xmlread) and carry the component keyword and the required 1.0.0 history.
%=============================================================================
catalog = NFlow.internal.acausalCatalog();
% Cover one representative component from each domain + a source and a sensor.
names = {'Resistor', 'Capacitor', 'SignalVoltage', 'VoltageSensor', ...
  'HeatCapacitor', 'Mass', 'IdealGear', 'Inertia'};
for i = 1:numel(names)
  for lang = {'en_US', 'fr_FR'}
    xml = NFlow.internal.acausalGenerateHelp(names{i}, lang{1});
    % Well-formed XML: write and xmlread it.
    p = [tempname(), '.xml'];
    fid = fopen(p, 'w'); fwrite(fid, xml); fclose(fid);
    doc = xmlread(p); % throws if malformed
    assert_istrue(~isempty(doc));
    % Required content.
    assert_istrue(~isempty(strfind(xml, ['<keyword>', names{i}, '</keyword>'])));
    assert_istrue(~isempty(strfind(xml, ['<keyword_alias>block:', names{i}, '</keyword_alias>'])));
    assert_istrue(~isempty(strfind(xml, ['<language>', lang{1}, '</language>'])));
    assert_istrue(~isempty(strfind(xml, '<history_version>1.0.0</history_version>')));
    assert_istrue(~isempty(strfind(xml, '<physicalPins')) == 0); % help, not manifest
  end
end
%=============================================================================
% Unknown component errors.
msg = '';
try
  NFlow.internal.acausalGenerateHelp('NoSuchComponent');
catch e
  msg = e.identifier;
end
assert_istrue(strcmp(msg, 'Nelson:nflow:acausalHelpUnknown'));
%=============================================================================
