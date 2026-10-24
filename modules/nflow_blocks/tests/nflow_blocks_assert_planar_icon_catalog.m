%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function nflow_blocks_assert_planar_icon_catalog(offset, stride)
  catalog = NFlow.internal.planarCatalog();
  for k = offset:stride:numel(catalog)
    svg = NFlow.internal.planarGenerateIcon(catalog(k).name);
    p = [tempname(), '.svg'];
    fid = fopen(p, 'w');
    fwrite(fid, svg);
    fclose(fid);
    doc = xmlread(p);
    assert_istrue(~isempty(doc));
    assert_istrue(~isempty(strfind(svg, 'viewBox="0 0 72 72"')));
    assert_istrue(~isempty(strfind(svg, '</svg>')));
  end
end
%=============================================================================
