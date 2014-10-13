#define will_beat(play) ("\001\002\000"[play])
#define will_lose_to(play) ("\002\000\001"[play])

/* ------------------------------------------------------------------------- */

static const int my_hist = 0,opp_hist = 1,both_hist = 2;

static int match_single(int i,int num,int *history) {
	int *highptr = history + num;
	int *lowptr = history + i;
	while (lowptr > history && *lowptr == *highptr) --lowptr, --highptr;
	return history + num - highptr;
}

static int match_both(int i,int num) {
	int j;
	for (j = 0; j < i && opp_history[num-j] == opp_history[i-j]
	                  && my_history[num-j]  == my_history[i-j]; ++j) ;
	return j;
}

static void do_history(int age,int best[3]) {
	const int num = my_history[0];
	int best_length[3],i,j,w;

	for (w = 0; w < 3; ++w) best[w] = best_length[w] = 0; 
	for (i = num - 1; i > num - age && i > best_length[my_hist]; --i) {
		j = match_single(i,num,my_history);
		if (j > best_length[my_hist]) {
			best_length[my_hist] = j;
			best[my_hist] = i;
			if (j > num / 2) break;
		}
	}

	for (i = num - 1; i > num - age && i > best_length[opp_hist]; --i) {
		j = match_single(i,num,opp_history);
		if (j > best_length[opp_hist]) {
			best_length[opp_hist] = j;
			best[opp_hist] = i;
			if (j > num / 2) break;
		}
	}

	for (i = num - 1; i > num - age && i > best_length[both_hist]; --i) {
		j = match_both(i,num);
		if (j > best_length[both_hist]) {
			best_length[both_hist] = j;
			best[both_hist] = i;
			if (j > num / 2) break;
		}
	}
}

/* ------------------------------------------------------------------------- */

struct stats {
	int sum[1 + trials][3];
	int age;
};

static void reset_stats(struct stats *st) {
	int i;
	st->age = 0;
	for (i = 0; i < 3; ++i) st->sum[st->age][i] = 0;
}

static void add_stats(struct stats *st,int i,int delta) {
	st->sum[st->age][i] += delta;
}

static void next_stats(struct stats *st) {
	if (st->age < trials) {
		int i;
		++(st->age);
		for (i = 0; i < 3; ++i) 
			st->sum[st->age][i] = st->sum[st->age - 1][i];
	}
}

static int max_stats(const struct stats *st,int age,int *which,int *score) {
	int i;
	*which = -1;
	for (i = 0; i < 3; ++i) {
		int diff;
		if (age > st->age) 
			diff = st->sum[st->age][i];
		else
			diff = st->sum[st->age][i] - st->sum[st->age - age][i];
		if (diff > *score) {
			*score = diff;
			*which = i;
		}
	}

	return -1 != *which;
}

/* ------------------------------------------------------------------------- */

struct predict {
	struct stats st;
	int last;
};

static void reset_predict(struct predict *pred) {
	reset_stats(&pred->st);
	pred->last = -1;
}

/* last: opponent's last move (-1 if none)
 | guess: algorithm's prediction of opponent's next move */
static void do_predict(struct predict *pred,int last,int guess) {
	if (-1 != last) {
		const int diff = (3 + last - pred->last) % 3;
		add_stats(&pred->st,will_beat(diff),1);
		add_stats(&pred->st,will_lose_to(diff),-1);
		next_stats(&pred->st);
	}

	pred->last = guess;
}

static void scan_predict(struct predict *pred,int age,int *move,int *score) {
	int i;
	if (max_stats(&pred->st,age,&i,score)) *move = ((pred->last + i) % 3);
}

/* ------------------------------------------------------------------------- */

static const int ages[] = { 1000, 100, 10, 5, 2, 1 };
#define num_ages (sizeof(ages) / sizeof(ages[0]))

struct iocaine {
	struct predict pr_history[num_ages][3][2],pr_freq[num_ages][2];
	struct predict pr_fixed,pr_random,pr_meta[num_ages];
	struct stats stats[2];
};

static int iocaine(struct iocaine *i) {
	const int num = my_history[0];
	const int last = (num > 0) ? opp_history[num] : -1;
	const int guess = biased_roshambo(1.0/3.0,1.0/3.0);
	int w,a,p;

	if (0 == num) {
		for (a = 0; a < num_ages; ++a) {
			reset_predict(&i->pr_meta[a]);
			for (p = 0; p < 2; ++p) {
				for (w = 0; w < 3; ++w)
					reset_predict(&i->pr_history[a][w][p]);
				reset_predict(&i->pr_freq[a][p]);
			}
		}
		for (p = 0; p < 2; ++p) reset_stats(&i->stats[p]);
		reset_predict(&i->pr_random);
		reset_predict(&i->pr_fixed);
	} else {
		add_stats(&i->stats[0],my_history[num],1);
		add_stats(&i->stats[1],opp_history[num],1);
	}

	for (a = 0; a < num_ages; ++a) {
		int best[3];
		do_history(ages[a],best);
		for (w = 0; w < 3; ++w) {
			const int b = best[w];
			if (0 == b) {
				do_predict(&i->pr_history[a][w][0],last,guess);
				do_predict(&i->pr_history[a][w][1],last,guess);
				continue;
			}
			do_predict(&i->pr_history[a][w][0],last,my_history[b+1]);
			do_predict(&i->pr_history[a][w][1],last,opp_history[b+1]);
		}

		for (p = 0; p < 2; ++p) {
			int best = -1,freq;
			if (max_stats(&i->stats[p],ages[a],&freq,&best))
				do_predict(&i->pr_freq[a][p],last,freq);
			else
				do_predict(&i->pr_freq[a][p],last,guess);
		}
	}

	do_predict(&i->pr_random,last,guess);
	do_predict(&i->pr_fixed,last,0);

	for (a = 0; a < num_ages; ++a) {
		int aa,score = -1,move = -1;
		for (aa = 0; aa < num_ages; ++aa) {
			for (p = 0; p < 2; ++p) {
				for (w = 0; w < 3; ++w)
					scan_predict(&i->pr_history[aa][w][p],
						     ages[a],&move,&score);
				scan_predict(&i->pr_freq[aa][p],ages[a],&move,&score);
			}
		}

		scan_predict(&i->pr_random,ages[a],&move,&score);
		scan_predict(&i->pr_fixed,ages[a],&move,&score);
		do_predict(&i->pr_meta[a],last,move);
	}

	{
		int score = -1,move = -1;
		for (a = 0; a < num_ages; ++a)
			scan_predict(&i->pr_meta[a],trials,&move,&score);
		return move;
	}
}

/* ------------------------------------------------------------------------- */

int iocainebot(void)
{
	static struct iocaine p;
	return iocaine(&p);
}
