%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
r = which('which', '-module');
if (strcmp(getenv('NELSON_PORTABLE_TEST_RUN'), '1') || iswasm())
  asserts.istrue(isempty(r{1}) || strcmp(r{1}, 'functions_manager'));
else
  asserts.isequal(r{1}, 'functions_manager');
end
