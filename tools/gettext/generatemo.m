%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if ispc()
  MSGFMT = ['"', nelsonroot(), '/tools/gettext/bin/msgfmt.exe', '"'];
else
  if ismac()
    MSGFMT = '/usr/local/bin/msgfmt';
    if ~isfile(MSGFMT)
      MSGFMT = 'msgfmt';
    end
  else
    MSGFMT = 'msgfmt';
  end
end

DOMAIN = 'nelson';

pot_en_US = [nelsonroot(), '/locale/nelson.pot'];
po_en_US = [nelsonroot(), '/locale/en_US/LC_MESSAGES/nelson.po'];
if ~isfile(po_en_US)
  status = mkdir([nelsonroot(), '/locale/en_US/LC_MESSAGES']);
  copyfile(pot_en_US, po_en_US);
end

langs = getavailablelanguages();

for l = langs(:)'
  posrc = [nelsonroot(), '/locale/', l{1}, '/LC_MESSAGES/', DOMAIN, '.po'];
  modst = [nelsonroot(), '/locale/', l{1}, '/LC_MESSAGES/', DOMAIN, '.mo'];
  dir_posrc = dir(posrc);
  dir_modst = dir(modst);
  needToUpdate = true;
  if ~isempty(dir_posrc)
    if ~isempty(dir_modst)
      if dir_modst.datenum > dir_posrc.datenum
        needToUpdate = false;
      end
    end
    if needToUpdate
      disp([_('Update translations:'), ' ', l{1}]);
      MSGFMT_CMD = [MSGFMT, ' --output-file="', modst, '" "', posrc, '"'];
      [r, errmsg] = unix(MSGFMT_CMD);
      if r != 0
        warning(errmsg);
      end
    else
      disp([_('Translations up-to-date:'), ' ', l{1}]);
    end
  end
end

exit('force')
%=============================================================================
