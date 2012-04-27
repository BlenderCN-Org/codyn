function plotcdn(output)
	global animation_state

	data = load(output);

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

	while 1
		if i >= size(x, 1)
			i = 1;
		end

		plot3(x(i, :), y(i, :), z(i, :), '-o');
		view(0, 0);

		xlim(dd(1, :));
		ylim(dd(2, :));
		zlim(dd(3, :));

		title(['t = ', sprintf('%.02f', nt(i))]);
		drawnow;

		pause(dt);

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
	global animation_state

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
	end
end
