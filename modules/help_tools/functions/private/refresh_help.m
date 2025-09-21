%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [tf, msg] = refresh_help(varargin)
  narginchk(0, 1);
  if nargin == 1
      mustBeTextScalar(varargin{1}, 1);
      modulename = varargin{1} ;
  else 
      modulename = '';
  end
  ver_number = version('-number');
  version_string = sprintf('%d.%d.%d', ver_number(1), ver_number(2), ver_number(3));
  help_dir = [userdir(), '/Nelson/', version_string, '/help/'];
  if ~isdir(help_dir)
    install_help(true);
  end
  modules_help_list = get_modules_help_list();
  if ~isempty(modulename)
    modules_help_list = [modules_help_list; modulename];
  end
   for lang = getavailablelanguages()'
    index_help_for_language([help_dir, lang{1}], lang{1}, modules_help_list, true);
  end
    tf = true;
    msg = '';
end
%=============================================================================
