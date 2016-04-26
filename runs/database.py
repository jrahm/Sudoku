import sqlite3
import glob

class Run:

    def __init__(self):

        self.n = 0
        self.hints = 0
        self.solution_grid = None
        self.input_grid = None
        self.output_grid = None
        self.time_millis = 0

    def __str__(self):
        return "{n = %d, hints = %d, solution = %s, input = %s, output = %s, time = %d}" % \
                (self.n, self.hints, self.solution_grid, self.input_grid, self.output_grid, self.time_millis)

    def insert(self, cursor):
        cursor.execute("insert into runs (n, hints, solution_grid, input_grid, output_grid, time_millis) values (%d, %d, %s, %s, %s, %d)" %\
                        (self.n, self.hints, self.solution_grid, self.input_grid, self.output_grid, self.time_millis))

def split_list(lst):

    ret = []
    current = []
    for l in lst:
        if l == '':
            if current != []:
                ret.append(current)
                current = []
        else:
            current.append(l)
    ret.append(current)
    return ret


def load_run(fname):

    try:
        _, n, nhints, number = fname.split('-')
        number, _ = number.split('.')
        fd = open(fname)

        ret_run = Run()

        lines = [x.strip() for x in fd.readlines()]
        lines = split_list(lines)

        ret_run.n = int(n)
        ret_run.hints = int(nhints)

        if lines[0][0] == 'Timeout':
            ret_run.solution_grid = 'NULL'
            ret_run.input_grid = 'NULL'
            ret_run.output_grid = 'NULL'
            ret_run.time_millis = 600000
        else:
            ret_run.solution_grid = "'" + ''.join(lines[0]) + "'"
            ret_run.input_grid = "'" + ''.join(lines[1]) + "'"
            ret_run.output_grid = "'" + ''.join(lines[2]) + "'"
            ret_run.time_millis = int(lines[3][0][6:])

        return ret_run


    except IOError as e:

        print(e)
        return None

connection = sqlite3.connect('database.db')
cursor = connection.cursor()

filenames = glob.glob('run*')
for fname in filenames:
    run = load_run(fname)
    print (run)
    run.insert(cursor)

connection.commit()
connection.close()

