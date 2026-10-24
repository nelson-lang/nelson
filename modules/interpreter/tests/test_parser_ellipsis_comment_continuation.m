%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
code = sprintf(['values = {''linear'', ... %% trailing comment\n', ...
  '''rbf'', ... %% second trailing comment\n', ...
  '''cosine''};']);
asserts.isequal(parsestring(code), 'script');
%=============================================================================
code = sprintf(['pairwise_kernel_functions = struct(...\n', ...
  '  ...''disabled'', @disabled_kernel,...\n', ...
  '  ''linear'', @linear_kernel,...\n', ...
  '  ...''other'', @other_kernel\n', ...
  '  ''poly'', @poly_kernel);']);
asserts.isequal(parsestring(code), 'script');
%=============================================================================
