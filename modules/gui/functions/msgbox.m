%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = msgbox(varargin)
  % h = msgbox('message')
  % h = msgbox('message', 'mode')
  % h = msgbox('message', 'title')
  % h = msgbox('message', 'title', 'mode')
  % h = msgbox('message', 'title', 'icon')
  % h = msgbox('message', 'title', 'icon', 'mode')

  if nargout > 2
    error(_('Wrong number of output arguments.'));
  end

  icon = 'none';
  title = '''';
  text = '''';
  mode = 'nonmodal';

  switch nargin()
    case 1
      text = varargin{1};

    case 2
      text = varargin{1};
      mode_or_title = varargin{2};
      if ismsgboxmode(mode_or_title)
        mode = mode_or_title;
      else
        title = mode_or_title;
      end

    case 3
      text = varargin{1};
      title = varargin{2};
      mode_or_icon = varargin{3};
      if ismsgboxmode(mode_or_icon)
        mode = mode_or_icon;
      else
        if ismsgboxicon(mode_or_icon)
           icon = mode_or_icon;
        else
           error(_('Wrong value for #3 argument. A valid icon or mode expected.'));
        end
      end

    case 4
      text = varargin{1};
      title = varargin{2};
      if ismsgboxicon(varargin{3})
        icon = varargin{3};
      else
        error(_('Wrong value for #3 argument. A valid icon expected.'));
      end
      if ismsgboxmode(varargin{4})
        mode = varargin{4};
      else
        error(_('Wrong value for #4 argument. A valid mode expected.'));
      end
    otherwise
      error(_('Wrong number of input arguments.'));
  end

  if ~ischar(text) && ~iscellstr(text)
    error(_('Wrong type for ''text'' argument. A string name expected.'));
  end

  if ~ischar(icon)
    error(_('Wrong type for ''icon'' argument. A icon name expected.'));
  end

  if ~ischar(title)
    error(_('Wrong type for ''title'' argument. A string expected.'));
  end

  if ~ischar(mode)
    error(_('Wrong type for ''mode'' argument. A string expected.'));
  end

  m = struct();
  m.title = '''''';
  m.text = '''''';
  m.visible = 'false';

  if strcmp(mode, 'nonmodal') == true
    m.modality = 'Qt.NonModal';
  end

  if strcmp(mode, 'modal') == true
    m.modality = 'Qt.WindowModal';
  end

  if strcmp(icon, 'none') == true
    m.icon = 'StandardIcon.NoIcon';
    objectName = 'msgbox';
    m.objectName = ['''', objectName, ''''];
  end

  if strcmp(icon, 'error') == true;
    m.icon = 'StandardIcon.Critical';
    objectName = 'errordlg';
    m.objectName = ['''', objectName, ''''];
  end

  if strcmp(icon, 'help') == true;
    m.icon = 'StandardIcon.Information';
    objectName = 'helpdlg';
    m.objectName = ['''', objectName, ''''];
  end

  if strcmp(icon, 'warn') == true;
    m.icon = 'StandardIcon.Warning';
    objectName = 'warndlg';
    m.objectName = ['''', objectName, ''''];
  end

  if strcmp(icon, 'question') == true;
    m.icon = 'StandardIcon.Question';
    objectName = 'questdlg';
    m.objectName = ['''', objectName, ''''];
  end

  bfound = false;
  if strcmp(mode, 'modal') == true || strcmp(mode, 'on') == true
    dlg_list = QObject_findchildren(QObject_root(),objectName, true);
    if ~isempty(dlg_list)
      for dlg = dlg_list
        if (strcmp(dlg.title, title) == true)
          h = dlg;
          bfound = true;
          break;
        end
      end
    end
  end

  if ~bfound
    qml_template = {'import QtQuick 2.2'
                    'import QtQuick.Dialogs 1.1'
                    ''
                    'MessageDialog {'};

    names = fieldnames(m);
    for n = fieldnames(m)
      v = getfield(m, n{1});
      if iscell(n)
        qml_template{end + 1} = ['   ', n{1}, ' : ', v];
      else
        qml_template{end + 1} = ['   ', n, ' : ', v];
      end
    end
    qml_template{end + 1} = '}';
    qml_box_as_txt = cellstr_to_txt(qml_template);
    h = qml_loadstring(qml_box_as_txt);
  else
    bfirst = true;
    for dlg = dlg_list
      if (strcmp(dlg.title, title) == true && bfirst)
        h = dlg;
        bfirst = false;
      else
        delete(dlg);
      end
    end
  end

  if iscellstr(text)
    h.text = cellstr_to_txt(text);
  else
    h.text = text;
  end

  h.title = title;
  h.visible = true;
  varargout{1} = h;
  if strcmp(mode, 'modal') == true
    h.modality = qt_constant('Qt.WindowModal');
    msgwait(h);
  else
    h.modality = qt_constant('Qt.NonModal');
  end

end
%=============================================================================
function msgwait(h)
  while(true)
    if (h.visible == false)
      break;
    end
    if h.clickedButton ~= 0
      break;
    end
    sleep(0.1);
  end
  delete(h);
end
%=============================================================================
function txt = cellstr_to_txt(cstr)
  array_text = char(cstr);
  txt = '';
  for k = 1:size(array_text, 1)
    if k == 1
      txt = [array_text(k, :)];
    else
      txt = [txt, char(10), array_text(k, :)];
    end
  end
end
%=============================================================================
function ok = ismsgboxmode(value)
  ok = strcmp(value, 'modal') == true || ...
       strcmp(value, 'nonmodal') == true || ...
       strcmp(value, 'on') == true;
end
%=============================================================================
function ok = ismsgboxicon(value)
  ok = (strcmp(value, 'none') == true || ...
      strcmp(value, 'error') == true || ...
      strcmp(value, 'help') == true || ...
      strcmp(value, 'warn') == true || ...
      strcmp(value, 'question') == true);
end
%=============================================================================
