boards = []
board_row_cols = []

f = open('dag4.txt', 'r')
alllines = f.readlines()  # put the lines to a variable (list).

draw_list = [int(x) for x in alllines[0].split(",")]
for i in range(2, len(alllines), 6):

    this_board = []

    for j in range(i, i + 5):
        string_row = alllines[j].replace("\n", "").split(" ")
        row = [int(x) for x in string_row if x != '']
        this_board.append(row)

    boards.append(this_board)
    board_row_cols.append(([0 for _ in range(5)], [0 for _ in range(5)]))


boards_finished = [False for _ in range(len(boards))]
total = len(boards)
winner_count = 0

def print_winner(x, draw):
    global winner_count

    if boards_finished[x]:
        return

    boards_finished[x] = True

    summ = 0
    for i in range(5):
        for j in range(5):
            if boards[x][i][j] >= 0:
                summ += boards[x][i][j]

    if winner_count == 0 or winner_count == total - 1:
        print(summ)
        print(draw)
        print("result=", summ * draw)
        print("winner total", winner_count, total)

    winner_count += 1


def do():
    for draw in draw_list:
        for x in range(len(boards)):
            for i in range(5):
                for j in range(5):
                    if boards[x][i][j] == draw:
                        board_row_cols[x][0][i] += 1
                        board_row_cols[x][1][j] += 1
                        boards[x][i][j] = -1

                        if board_row_cols[x][0][i] == 5:
                            print_winner(x, draw)
                        if board_row_cols[x][1][j] == 5:
                            print_winner(x, draw)


do()
