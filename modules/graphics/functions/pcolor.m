function varargout = pcolor(varargin)
  inputArguments = varargin;
  if (length(inputArguments) >= 2)
     if ((length(inputArguments{1})==1) && isgraphics(inputArguments{1}, 'axes'))
        ax = inputArguments{1}(1);
        inputArguments = inputArguments(2:end);
     else   
        ax = newplot();
     end
  else
    ax = newplot();
  end
  nextPlot = ax.NextPlot;
  ax = cla(ax);
   
  if (length(inputArguments) == 1)
    C = inputArguments{1};
    h = surface(ax, C);
    [m, n] = size(C);
    xlims = [1, n];
    ylims = [1, m];
  elseif (length(inputArguments) == 3)
    X = inputArguments{1};
    Y = inputArguments{2};
    C = inputArguments{3};
    [nr, nc] = size(C);
    Z = zeros (nr, nc);
    h = surface(ax, X, Y, Z, C);
    xlims = [min(min(X)), max(max(X))];
    ylims = [min(min(Y)), max(max(Y))];
  else
    error(_('pcolor requires either X,Y,C, or C arguments.'));
  end
  axis('tight');
  if ismember(nextPlot, {'replace', 'replaceall'})
    if xlims(2) <= xlims(1)
        xlims(2) = xlims(1) + 1;
    end
    if ylims(2) <= ylims(1)
        ylims(2) = ylims(1) + 1;
    end
    xlim(ax, xlims);
    ylim(ax, ylims);
  end  
  if (nargout > 0)
    varargout{1} = h;
  end
end
