function plotcdn(dirname)
	global animation_state multiplier

	if nargin < 1
		dirname = '.';
	end

	data = scan_log(fullfile(dirname, 'kinematics.txt'));

	figure(1)
	h1 = subplot(4, 1, 1);
	plot(data.time, data.alldata(:, 2:end));
	title('Joint Angles/Velocities');
	names = fieldnames(data);
	l = legend(names{2:end - 1});
	set(l, 'Interpreter', 'none');

	data = scan_log(fullfile(dirname, 'torques.txt'));
	h2 = subplot(4, 1, 4);
	plot(data.time, data.alldata(:, 2:end));
	title('Joint Torques');
	names = fieldnames(data);
	l = legend(names{2:end - 1});
	set(l, 'Interpreter', 'none');

	data = scan_log(fullfile(dirname, 'learning.txt'));
	h3 = subplot(4, 1, 2);
	plot(data.time, data.alldata(:, 2:end));
	title('Learning');
	names = fieldnames(data);
	l = legend(names{2:end - 1});
	set(l, 'Interpreter', 'none');

	h4 = subplot(4, 1, 3);

	toterr = sum(abs(data.alldata(:, 3:2:end)), 2);
	toterr = filter(zeros(1, 100) + 0.01 , 1, toterr);
	plot(data.time, toterr);
	title('Learning error');
	legend('Average error');

	linkaxes([h1, h2, h3, h4], 'x');
	realign(4, 1);

	figure(3)
	visual = fullfile(dirname, 'visual.txt');
	data = dlmread(visual, '', 1, 0);

	t = data(:, 1);

	x = data(:, 2:3:end);
	y = data(:, 3:3:end);
	z = data(:, 4:3:end);

	fs = 30;
	dt = 1 / fs;

	nt = 0:dt:t(end);

	x = interp1(t, x, nt);
	y = interp1(t, y, nt);
	z = interp1(t, z, nt);

	dx = [min(x(:)), max(x(:))];
	dy = [min(y(:)), max(y(:))];
	dz = [min(z(:)), max(z(:))];

	ay = (dy(2) - dy(1)) / 2;
	ax = (dx(2) - dx(1)) / 2;
	az = (dz(2) - dz(1)) / 2;

	aa = [ax; ay; az];
	dd = [dx; dy; dz];

	[~, i] = max(dd(:, 2) - dd(:, 1));
	ir = find([1, 2, 3] ~= i);

	aa(ir) = dd(ir, 1) + aa(ir);
	dd(ir, :) = [aa(ir) - aa(i), aa(ir) + aa(i)]; % + [-0.1, 0.1; -0.1, 0.1];

	set(gcf, 'KeyReleaseFcn', @animation_key);
	animation_state = 0;

	i = 1;
	h = gca;
	multiplier = 1;

	while 1
		if i >= size(x, 1)
			i = 1;
		end

		plot3(h, x(i, :), y(i, :), z(i, :), '-o');
		view(h, 0, 0);

		xlim(h, dd(1, :));
		ylim(h, dd(2, :));
		zlim(h, dd(3, :));

		title(h, ['t = ', sprintf('%.02f', nt(i))]);
		drawnow;

		pause(dt / multiplier);

		if animation_state == 1
			break
		elseif animation_state == 2
			continue;
		elseif animation_state == 3
			i = 0;
			animation_state = 2;
		end

		i = i + 1;
	end

	if animation_state == 1
		close(gcf);
	end
end

function animation_key(f, ev)
	global animation_state multiplier

	if strcmp(ev.Key, 'escape')
		animation_state = 1;
	elseif strcmp(ev.Key, 'space')
		if animation_state == 2
			animation_state = 0;
		else
			animation_state = 2;
		end
	elseif strcmp(ev.Key, 'r')
		animation_state = 3;
	elseif strcmp(ev.Key, 'rightarrow')
		multiplier = multiplier * 1.5;
	elseif strcmp(ev.Key, 'leftarrow')
		multiplier = multiplier / 1.5;
	end
end
