#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
std::vector<double> bout_end(std::vector<int> seq, std::vector<double> activity, std::vector<double> interval, int size, int window_size) {
  double dwindow_size = window_size - 0.0001;
	std::vector<double> end_pos(size * 3);
	for (int i = 0; i < size; i++) {
		int step_count = 0;
		int inner_pos = i;
		double bout_duration = 0;
		double stepping_duration = 0;
		if (activity[i] == 2 || activity[i] == 1) {
			while ((activity[inner_pos] == 2 || activity[inner_pos] == 1) && bout_duration < dwindow_size && inner_pos < size) {
				// Next activity is standing or stepping and current bout is shorter than the window.
				// Add the next event to the bout
				bout_duration += interval[inner_pos];
				if (activity[inner_pos] == 2) {
					step_count += 2;
					stepping_duration += interval[inner_pos];
				}
				inner_pos += 1;
			}
			if (activity[inner_pos - 1] == 1 && bout_duration > dwindow_size) {
				// Last event of interest is a period of quiet standing.
				// If the quiet standing makes the bout longer than the window then set the total bout duration to equal the window size
				bout_duration = window_size;
			}
		}
		else {
			inner_pos = -1;
		}
		end_pos[i] = inner_pos;
		end_pos[size + i] = step_count;
		end_pos[(2 * size) + i] = stepping_duration;
	}
	return end_pos;
}
