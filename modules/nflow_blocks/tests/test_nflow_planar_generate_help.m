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
% Acausal palette N7: NFlow.internal.planarGenerateHelp emits a well-formed Nelson help
% XML page per planar block (en + fr), generated from NFlow.internal.planarCatalog. Each
% page must parse (xmlread) and carry the block keyword and the required 1.0.0
% history. Every catalog block must produce a parseable page.
%=============================================================================
catalog = NFlow.internal.planarCatalog();
assert_istrue(numel(catalog) >= 12);
for k = 1:numel(catalog)
  for lang = {'en_US', 'fr_FR'}
    name = catalog(k).name;
    xml = NFlow.internal.planarGenerateHelp(name, lang{1});
    p = [tempname(), '.xml'];
    fid = fopen(p, 'w'); fwrite(fid, xml); fclose(fid);
    doc = xmlread(p); % throws if malformed
    assert_istrue(~isempty(doc));
    assert_istrue(~isempty(strfind(xml, ['<keyword>', name, '</keyword>'])));
    assert_istrue(~isempty(strfind(xml, ['<keyword_alias>block:', name, '</keyword_alias>'])));
    assert_istrue(~isempty(strfind(xml, ['<language>', lang{1}, '</language>'])));
    assert_istrue(~isempty(strfind(xml, '<history_version>1.0.0</history_version>')));
    assert_istrue(~isempty(strfind(xml, '<xmldoc>')));
    assert_istrue(~isempty(strfind(xml, '</xmldoc>')));
  end
end
%=============================================================================
% Sensor blocks advertise a signal output; structural blocks do not.
xmlSensor = NFlow.internal.planarGenerateHelp('PlanarPositionSensor', 'en_US');
assert_istrue(~isempty(strfind(xmlSensor, 'one signal output port')));
xmlBody = NFlow.internal.planarGenerateHelp('PlanarBody', 'en_US');
assert_istrue(~isempty(strfind(xmlBody, 'structural block')));
%=============================================================================
% Unknown block errors with the documented identifier.
msg = '';
try
  NFlow.internal.planarGenerateHelp('NoSuchPlanarBlock');
catch e
  msg = e.identifier;
end
assert_istrue(strcmp(msg, 'Nelson:nflow:planarHelpUnknown'));
%=============================================================================
