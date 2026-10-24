%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function plan = analyze(options)
  narginchk(1, 1);
  nargoutchk(0, 1);
  args = __nelsonc_option_arguments__(options);
  args{end + 1} = '--explain-link';
  plan = __nelsonc_command__(args, pwd(), nargout() == 0);
end
