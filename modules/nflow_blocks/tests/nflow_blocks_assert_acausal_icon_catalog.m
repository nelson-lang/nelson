%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function nflow_blocks_assert_acausal_icon_catalog(offset, stride)
  catalog = NFlow.internal.acausalCatalog();
  for k = offset:stride:numel(catalog)
    name = catalog(k).name;
    svg = NFlow.internal.acausalGenerateIcon(name);
    p = [tempname(), '.svg'];
    fid = fopen(p, 'w');
    fwrite(fid, svg);
    fclose(fid);
    doc = xmlread(p);
    assert_istrue(~isempty(doc));
    % documentation-icon layout: one unit per pixel, viewBox = graphics bounding box
    assert_istrue(~isempty(strfind(svg, 'viewBox="')));
    assert_istrue(~isempty(strfind(svg, 'transform="matrix(1 0 0 -1 0 0)"')));
    assert_istrue(~isempty(strfind(svg, '</svg>')));
    % The Modelica icon carries its instance-name label, rendered as the block name.
    assert_istrue(~isempty(strfind(svg, ['>', name, '</text>'])));
  end
end
%=============================================================================
