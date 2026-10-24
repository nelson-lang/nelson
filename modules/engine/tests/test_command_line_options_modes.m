%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
binpath = modulepath('nelson', 'bin');
nelson_cli = [binpath, '/nelson-cli'];
nelson_adv_cli = [binpath, '/nelson-adv-cli'];
%=============================================================================
if ~ismac()
  [status, output] = run_nelson_cli(nelson_cli, ' --cli');
  asserts.notEqual(status, 0);
  asserts.istrue(contains(output, 'Nelson:ProgramOptions:ModeSelector'));
  asserts.istrue(contains(output, 'mode selector option is only valid for the nelson launcher'));
end
%=============================================================================
