%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% The build-only compiler module stays out of deployed applications: their
% native launcher sets the extraction directory before any module loads.
if isempty(getenv('NELSONC_EXTRACTION_DIRECTORY'))
  addmodule(fullfile(nelsonroot(), 'modules', 'compiler'), 'compiler');
end
%=============================================================================