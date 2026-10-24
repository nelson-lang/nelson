%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function result = build(options)
  narginchk(1, 1);
  nargoutchk(0, 1);
  args = __nelsonc_option_arguments__(options);
  if nargout() == 0
    __nelsonc_command__(args, pwd(), options.Verbose);
    return;
  end
  report = __nelsonc_command__(args, pwd(), options.Verbose, true);
  result = nelson.compiler.BuildResult(report, options);
end
