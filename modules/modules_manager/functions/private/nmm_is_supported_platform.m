%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [r, msg] = nmm_is_supported_platform(plaforms)
  r = false;
  msg = sprintf(_('Platform not supported: %s'), sprintf('%s ', string(plaforms')));
  validPlatforms = ["all", "win32" , "win64", "maci64", "maci32", "glnxa64", "glnxa32"];
  if all(contains(plaforms, validPlatforms))
    currentArch = computer('arch');
  else
    return
  end
  currentArch = computer('arch');
  if ischar(plaforms)
    if any(contains(validPlatforms, plaforms))
      if strcmp(plaforms, "all")
        r = true;
        msg = '';
      else
        if strcmp(plaforms, currentArch)
          r = true;
        else
          r = false;
          msg = sprintf(_('Platform not supported: %s'), plaforms);
        end
      end
    else
      r = false;
      msg = sprintf(_('Platform not supported: %s'), plaforms);
    end
  else
    if iscell(plaforms)
      if any(contains(validPlatforms, plaforms))
        r = true;
        msg = '';
      else
        r = false;
        msg = sprintf(_('Platform not supported: %s'), plaforms);
      end
    else
      msg = _('Invalid type in platforms');
    end
  end
end
%=============================================================================
