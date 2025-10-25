%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [tf, msg] = uninstall_help()
  ver_number = version('-number');
  version_string = sprintf('%d.%d.%d', ver_number(1), ver_number(2), ver_number(3));
  help_dir = [userdir(), '/Nelson/', version_string, '/help/'];
  if ~isdir(help_dir)
    tf = false;
    msg = _('Help directory does not exist.');
    return;
  end
  [tf, msg] = rmdir(help_dir, 's');
end
%=============================================================================
