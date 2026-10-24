%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function nflow_blocks_assert_acausal_help_catalog(offset, stride)
  catalog = NFlow.internal.acausalCatalog();
  for k = offset:stride:numel(catalog)
    xml = NFlow.internal.acausalGenerateHelp(catalog(k).name, 'en_US');
    assert_istrue(~isempty(strfind(xml, '<xmldoc>')));
    assert_istrue(~isempty(strfind(xml, '</xmldoc>')));
  end
end
%=============================================================================
