class Result:
    def __init__(self, success=False, payload=None, rest=None):
        self.success = success
        self.data = payload
        self.rest = rest


class TreeNode:
    def __init__(self, content, left, right):
        self.content = content
        self.left = left
        self.right = right

    def toString(self):
        lstring = self.left.toString() if self.left else ""
        rstring = self.right.toString() if self.right else ""
        return "(" + lstring + ")" + str(self.content) + "(" + rstring + ")"

    def __str__(self):
        return self.toString()

    def insert(self, x):
        if x <= self.content:
            if self.left is None:
                self.left = TreeNode(x, None, None)
            else:
                self.left.insert(x)
        else:
            if self.right is None:
                self.right = TreeNode(x, None, None)
            else:
                self.right.insert(x)
        return self

    def invert(self):
        self.left, self.right = self.right, self.left
        if self.left: self.left.invert()
        if self.right: self.right.invert()
        return self


    @classmethod
    def from_string(cls, s):
        def integer(s):
            c = 0
            while c < len(s) and s[c].isdigit():
                c += 1
            return Result(True, int(s[:c]), s[c:]) if c > 0 else Result(False, None, s)

        def nothing(s):
            return Result(True,None,s)

        def terminal(x):
            def anon(s):
                if s[0] == x:
                    return Result(True, x, s[1:])
                return Result(False, None, s)
            return anon

        # input: array of parsers
        # output: return function f which takes input string s
        # and returns the result of the first parser who succeeds on s
        def OR(arr):
            def anon(s):
                for p in arr:
                    if (r := p(s)).success:
                        return r
                return Result(False,None,s)
            return anon

        def AND(arr):
            def anon(s):
                ret = []
                i = s
                for parser in arr:
                    result = parser(i)
                    if not result.success: return Result(False, None, s)
                    ret.append(result.data)
                    i = result.rest

                return Result(True,ret,i)
            return anon

        # exp -> (exp) int (exp) | int | [empty string]
        def exp(s):
            subexp = OR([integer, exp, nothing])
            result = AND([terminal("("), subexp, terminal(")"), integer, terminal("("), subexp, terminal(")")])(s)
            if not result.success: return Result(False, None, s)
            _, l, _, c, _, r, _ = result.data
            return Result(True, TreeNode(c, l, r), result.rest)
        return exp(s).data






tree = TreeNode(7, None, None)
tree.insert(3)
tree.insert(1)
tree.insert(4)
tree.insert(9)
tree.insert(8)
tree.insert(20)
tree.insert(11)
tree.insert(11)
tree.insert(12)
tree.insert(0)
tree.insert(5)
print(tree)
print(TreeNode.from_string(tree.toString()))
print(TreeNode.from_string(tree.toString()).invert())
