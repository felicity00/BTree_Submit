#include "utility.hpp"
#include <functional>
#include <cstddef>
#include "exception.hpp"
#include <cstdio>
namespace sjtu {
	//FILE *fp;
	constexpr char BPTREE_ADDRESS[128] = "data.file";
	template <class Key, class Value, class Compare = std::less<Key> >
	class BTree {
	private:
		// Your private members go here
		static FILE* fp;

		struct File_Data {
            off_t block_cnt = 1;
			off_t root_pos = 0;
			off_t head = 0;
			off_t rear = 0;
			off_t num = 0;
		};

		struct Block_Data {
			bool block_type = false;
			off_t num = 0;
			off_t pos = 0;
			off_t parent = 0;
			off_t last = 0;
			off_t next = 0;
		};

		struct NODE_DATA {
			off_t child = 0;
			Key key;
		};

		struct LEAF_DATA {
		    Key key;
		    Value value;
		};



		constexpr static off_t BLOCK_SIZE = 4096;
		constexpr static off_t HEAD_SIZE = sizeof(Block_Data);
		constexpr static off_t M = (BLOCK_SIZE - HEAD_SIZE) / sizeof(NODE_DATA) - 1;
		constexpr static off_t L = (BLOCK_SIZE - HEAD_SIZE) / sizeof(LEAF_DATA) - 1;


		struct Node_Data {

			NODE_DATA val[M];
		};

		struct Leaf_Data {

			LEAF_DATA val[L];
		};

		File_Data tree_data;


		static void mem_read(void* buff, off_t buff_size, off_t pos) {
			fseek(fp, long(buff_size * pos), SEEK_SET);
			fread(buff, buff_size, 1, fp);
		}

		static void mem_write(void* buff, off_t buff_size, off_t pos) {
			fseek(fp, long(buff_size * pos), SEEK_SET);
			fwrite(buff, buff_size, 1, fp);
			fflush(fp);
		}

		static void read_node(Block_Data* info, Node_Data* data, off_t pos)
		{
			char buff[BLOCK_SIZE] = { 0 };
			mem_read(buff, BLOCK_SIZE, pos);
			memcpy(info, buff, sizeof(Block_Data));
			memcpy(data, buff + HEAD_SIZE, sizeof(Node_Data));
		}

		static void read_leaf(Block_Data* info, Leaf_Data* data, off_t pos)
		{
			char buff[BLOCK_SIZE] = { 0 };
			mem_read(buff, BLOCK_SIZE, pos);
			memcpy(info, buff, sizeof(Block_Data));
			memcpy(data, buff + HEAD_SIZE, sizeof(Leaf_Data));
		}

		static void write_node(Block_Data* info, Node_Data* data, off_t pos) {
			char buff[BLOCK_SIZE] = { 0 };
			memcpy(buff, info, sizeof(Block_Data));
			memcpy(buff + HEAD_SIZE, data, sizeof(Node_Data));
			mem_write(buff, BLOCK_SIZE, pos);
		}

		static void write_leaf(Block_Data* info, Leaf_Data* data, off_t pos) {
			char buff[BLOCK_SIZE] = { 0 };
			memcpy(buff, info, sizeof(Block_Data));
			memcpy(buff + HEAD_SIZE, data, sizeof(Leaf_Data));
			mem_write(buff, BLOCK_SIZE, pos);
		}

		void write_tree_data() {
			fseek(fp, 0, SEEK_SET);
			char buff[BLOCK_SIZE] = { 0 };
			memcpy(buff, &tree_data, sizeof(tree_data));
			mem_write(buff, BLOCK_SIZE, 0);
		}

		//获取新内存
		off_t memory_allocation() {
			++tree_data.block_cnt;
			write_tree_data();
			char buff[BLOCK_SIZE] = { 0 };
			mem_write(buff, BLOCK_SIZE, tree_data.block_cnt - 1);
			return tree_data.block_cnt - 1;
		}

		//创建新的索引结点
		off_t create_node(off_t parent) {
			off_t node_pos = memory_allocation();
			Block_Data temp;
			Node_Data normal_data;
			temp.block_type = false;
			temp.parent = parent;
			temp.pos = node_pos;
			temp.num = 0;
			write_node(&temp, &normal_data, node_pos);
			return node_pos;
		}

		off_t create_leaf(off_t parent, off_t last, off_t next) {
			off_t node_pos = memory_allocation();
			Block_Data temp;
			Leaf_Data leaf_data;
			temp.block_type = true;
			temp.parent = parent;
			temp.pos = node_pos;
			temp.last = last;
			temp.next = next;
			temp.num = 0;
			write_leaf(&temp, &leaf_data, node_pos);
			return node_pos;
		}

		void insert_new_index(Block_Data& parent_info, Node_Data& parent_data,off_t origin, off_t new_pos, const Key& new_index) {
			++parent_info.num;
			auto p = parent_info.num - 2;
			while (parent_data.val[p].child != origin) {
				parent_data.val[p + 1] = parent_data.val[p];
				--p;
			}
			parent_data.val[p + 1].key = parent_data.val[p].key;
			parent_data.val[p].key = new_index;
			parent_data.val[p + 1].child = new_pos;
		}




		void check_file() {
			if (!fp) {
				fp = fopen(BPTREE_ADDRESS, "wb+");
				write_tree_data();

				off_t node_head = tree_data.block_cnt,
					node_rear = tree_data.block_cnt + 1;

				tree_data.head = node_head;
				tree_data.rear = node_rear;

				create_leaf(0, 0, node_rear);
				create_leaf(0, node_head, 0);

				return;
			}
			char buff[BLOCK_SIZE] = { 0 };
			mem_read(buff, BLOCK_SIZE, 0);
			memcpy(&tree_data, buff, sizeof(tree_data));
		}



		Key split_leaf_node(off_t pos, Block_Data& origin_info, Leaf_Data& origin_data) {

			off_t parent_pos;
			Block_Data parent_info;
			Node_Data parent_data;

			if (pos == tree_data.root_pos) {
				off_t root_pos = create_node(0);
				tree_data.root_pos = root_pos;
				write_tree_data();
				read_node(&parent_info, &parent_data, root_pos);
				origin_info.parent = root_pos;
				++parent_info.num;
				parent_data.val[0].child = pos;
				parent_pos = root_pos;
			}
			else {
				read_node(&parent_info, &parent_data, origin_info.parent);
				parent_pos = parent_info.pos;
			}
			if (split_parent(origin_info)) {
				parent_pos = origin_info.parent;
				read_node(&parent_info, &parent_data, parent_pos);
			}

			off_t new_pos = create_leaf(parent_pos,pos,origin_info.next);

			off_t tmp_pos = origin_info.next;
			Block_Data tmp_info;
			Leaf_Data tmp_data;
			read_leaf(&tmp_info, &tmp_data, tmp_pos);
			tmp_info.last = new_pos;
			write_leaf(&tmp_info, &tmp_data, tmp_pos);
			origin_info.next = new_pos;

			Block_Data new_info;
			Leaf_Data new_data;
			read_leaf(&new_info, &new_data, new_pos);
			off_t mid_pos = origin_info.num >> 1;
			for (off_t p = mid_pos, i = 0; p < origin_info.num; ++p, ++i) {
				new_data.val[i] = origin_data.val[p];
				++new_info.num;
			}
			origin_info.num = mid_pos;
			insert_new_index(parent_info, parent_data, pos, new_pos, origin_data.val[mid_pos].key);

			write_leaf(&origin_info, &origin_data, pos);
			write_leaf(&new_info, &new_data, new_pos);
			write_node(&parent_info, &parent_data, parent_pos);

			return new_data.val[0].key;
		}

		bool split_parent(Block_Data& child_info) {

			off_t parent_pos, origin_pos = child_info.parent;
			Block_Data parent_info, origin_info;
			Node_Data parent_data, origin_data;
			read_node(&origin_info, &origin_data, origin_pos);
			if (origin_info.num < M)
				return false;

			if (origin_pos == tree_data.root_pos) {
				off_t root_pos = create_node(0);
				tree_data.root_pos = root_pos;
				write_tree_data();
				read_node(&parent_info, &parent_data, root_pos);
				origin_info.parent = root_pos;
				++parent_info.num;
				parent_data.val[0].child = origin_pos;
				parent_pos = root_pos;
			}
			else {
				read_node(&parent_info, &parent_data, origin_info.parent);
				parent_pos = parent_info.pos;
			}
			if (split_parent(origin_info)) {
				parent_pos = origin_info.parent;
				read_node(&parent_info, &parent_data, parent_pos);
			}

			off_t new_pos = create_node(parent_pos);
			Block_Data new_info;
			Node_Data new_data;
			read_node(&new_info, &new_data, new_pos);

			off_t mid_pos = origin_info.num >> 1;
			for (off_t p = mid_pos + 1, i = 0; p < origin_info.num; ++p,++i) {
				if (origin_data.val[p].child == child_info.pos) {
					child_info.parent = new_pos;
				}
				NODE_DATA tmp=origin_data.val[i];
				origin_data.val[p] = new_data.val[i];
				new_data.val[i] = tmp;
				//std::swap(new_data.val[i], origin_data.val[p]);
				++new_info.num;
			}
			origin_info.num = mid_pos + 1;
			insert_new_index(parent_info, parent_data, origin_pos, new_pos, origin_data.val[mid_pos].key);

			//写入
			write_node(&origin_info, &origin_data, origin_pos);
			write_node(&new_info, &new_data, new_pos);
			write_node(&parent_info, &parent_data, parent_pos);
			return true;
		}


	public:
		typedef pair<const Key, Value> value_type;

		class const_iterator;
		class iterator {
			friend class sjtu::BTree<Key, Value, Compare>::const_iterator;
			friend iterator sjtu::BTree<Key, Value, Compare>::begin();
			friend iterator sjtu::BTree<Key, Value, Compare>::end();
			friend iterator sjtu::BTree<Key, Value, Compare>::find(const Key&);
			friend pair<iterator, OperationResult> sjtu::BTree<Key, Value, Compare>::insert(const Key&, const Value&);
		private:
			// Your private members go here
			BTree* cur_bptree = nullptr;
			Block_Data block_info;
			off_t cur_pos = 0;

		public:
			bool modify(const Value& value) {
				Block_Data info;
				Leaf_Data leaf_data;
				read_block(&info, &leaf_data, block_info.pos);
				leaf_data.val[cur_pos].second = value;
				write_block(&info, &leaf_data, block_info.pos);
				return true;
			}
			iterator() {
				// TODO Default Constructor
			}
			iterator(const iterator& other) {
				// TODO Copy Constructor
				cur_bptree = other.cur_bptree;
				block_info = other.block_info;
				cur_pos = other.cur_pos;
			}
			// Return a new iterator which points to the n-next elements
			iterator operator++(int) {
				// Todo iterator++
				auto tmp = *this;
				++cur_pos;
				if (cur_pos >= block_info.size) {
					char buff[BLOCK_SIZE] = { 0 };
					mem_read(buff, BLOCK_SIZE, block_info.next);
					memcpy(&block_info, buff, sizeof(block_info));
					cur_pos = 0;
				}
				return tmp;
			}
			iterator& operator++() {
				// Todo ++iterator
				++cur_pos;
				if (cur_pos >= block_info.num) {
					char buff[BLOCK_SIZE] = { 0 };
					mem_read(buff, BLOCK_SIZE, block_info.next);
					memcpy(&block_info, buff, sizeof(block_info));
					cur_pos = 0;
				}
				return *this;
			}
			iterator operator--(int) {
				// Todo iterator--
				auto temp = *this;
				if (cur_pos == 0) {
					char buff[BLOCK_SIZE] = { 0 };
					mem_read(buff, BLOCK_SIZE, block_info.last);
					memcpy(&block_info, buff, sizeof(block_info));
					cur_pos = block_info.num - 1;
				}
				else
					--cur_pos;
				return temp;
			}
			iterator& operator--() {
				// Todo --iterator
				if (cur_pos == 0) {
					char buff[BLOCK_SIZE] = { 0 };
					mem_read(buff, BLOCK_SIZE, block_info.last);
					memcpy(&block_info, buff, sizeof(block_info));
					cur_pos = block_info.num - 1;
				}
				else
					--cur_pos;

				return *this;
			}
			// Overloaded of operator '==' and '!='
			// Check whether the iterators are same
			value_type operator*() const {
				// Todo operator*, return the <K,V> of iterator
				if (cur_pos >= block_info.num)
					throw invalid_iterator();
				char buff[BLOCK_SIZE] = { 0 };
				mem_read(buff, BLOCK_SIZE, block_info.pos);
				Leaf_Data leaf_data;
				memcpy(&leaf_data, buff + HEAD_SIZE, sizeof(leaf_data));
				value_type result(leaf_data.val[cur_pos].first,leaf_data.val[cur_pos].second);
				return result;
			}
			bool operator==(const iterator& rhs) const {
				// Todo operator ==
				return cur_bptree == rhs.cur_bptree
					&& block_info.pos == rhs.block_info.pos
					&& cur_pos == rhs.cur_pos;
			}
			bool operator==(const const_iterator& rhs) const {
				// Todo operator ==
				return block_info.pos == rhs.block_info.pos
					&& cur_pos == rhs.cur_pos;
			}
			bool operator!=(const iterator& rhs) const {
				// Todo operator !=
				return cur_bptree != rhs.cur_bptree
					|| block_info.pos != rhs.block_info.pos
					|| cur_pos != rhs.cur_pos;
			}
			bool operator!=(const const_iterator& rhs) const {
				// Todo operator !=
				return block_info.pos != rhs.block_info.pos
					|| cur_pos != rhs.cur_pos;
			}
		};
		class const_iterator {
			// it should has similar member method as iterator.
			//  and it should be able to construct from an iterator.
			friend class sjtu::BTree<Key, Value, Compare>::iterator;
			friend const_iterator sjtu::BTree<Key, Value, Compare>::cbegin() const;
			friend const_iterator sjtu::BTree<Key, Value, Compare>::cend() const;
			friend const_iterator sjtu::BTree<Key, Value, Compare>::find(const Key&) const;
		private:
			// Your private members go here
			Block_Data block_info;
			off_t cur_pos = 0;
		public:
			const_iterator() {
				// TODO
			}
			const_iterator(const const_iterator& other) {
				// TODO
				block_info = other.block_info;
				cur_pos = other.cur_pos;
			}
			const_iterator(const iterator& other) {
				// TODO
				block_info = other.block_info;
				cur_pos = other.cur_pos;
			}
			// And other methods in iterator, please fill by yourself.
			// Return a new iterator which points to the n-next elements
			const_iterator operator++(int) {
				// Todo iterator++
				auto tmp = *this;
				++cur_pos;
				if (cur_pos >= block_info.size) {
					char buff[BLOCK_SIZE] = { 0 };
					mem_read(buff, BLOCK_SIZE, block_info.next);
					memcpy(&block_info, buff, sizeof(block_info));
					cur_pos = 0;
				}
				return tmp;
			}
			const_iterator& operator++() {
				// Todo ++iterator
				++cur_pos;
				if (cur_pos >= block_info.size) {
					char buff[BLOCK_SIZE] = { 0 };
					mem_read(buff, BLOCK_SIZE, block_info.next);
					memcpy(&block_info, buff, sizeof(block_info));
					cur_pos = 0;
				}
				return *this;
			}
			const_iterator operator--(int) {
				// Todo iterator--
				auto tmp = *this;
				if (cur_pos == 0) {
					char buff[BLOCK_SIZE] = { 0 };
					mem_read(buff, BLOCK_SIZE, block_info.last);
					memcpy(&block_info, buff, sizeof(block_info));
					cur_pos = block_info.size - 1;
				}
				else
					--cur_pos;
				return tmp;
			}
			const_iterator& operator--() {
				// Todo --iterator
				if (cur_pos == 0) {
					char buff[BLOCK_SIZE] = { 0 };
					mem_read(buff, BLOCK_SIZE, block_info.last);
					memcpy(&block_info, buff, sizeof(block_info));
					cur_pos = block_info.size - 1;
				}
				else
					--cur_pos;

				return *this;
			}
			// Overloaded of operator '==' and '!='
			// Check whether the iterators are same
			value_type operator*() const {
				// Todo operator*, return the <K,V> of iterator
				if (cur_pos >= block_info.size)
					throw invalid_iterator();
				char buff[BLOCK_SIZE] = { 0 };
				mem_read(buff, BLOCK_SIZE, block_info.pos);
				Leaf_Data leaf_data;
				memcpy(&leaf_data, buff + HEAD_SIZE, sizeof(leaf_data));
				value_type result(leaf_data.val[cur_pos].first, leaf_data.val[cur_pos].second);
				return result;
			}
			bool operator==(const iterator& rhs) const {
				// Todo operator ==
				return block_info.pos == rhs.block_info.pos
					&& cur_pos == rhs.cur_pos;
			}
			bool operator==(const const_iterator& rhs) const {
				// Todo operator ==
				return block_info.pos == rhs.block_info.pos
					&& cur_pos == rhs.cur_pos;
			}
			bool operator!=(const iterator& rhs) const {
				// Todo operator !=
				return block_info.pos != rhs.block_info.pos
					|| cur_pos != rhs.cur_pos;
			}
			bool operator!=(const const_iterator& rhs) const {
				// Todo operator !=
				return block_info.pos != rhs.block_info.pos
					|| cur_pos != rhs.cur_pos;
			}
		};
		// Default Constructor and Copy Constructor
		BTree() {
			// Todo Default
			fp = fopen(BPTREE_ADDRESS, "rb+");
			if (!fp) {
				fp = fopen(BPTREE_ADDRESS, "wb+");
				write_tree_data();

				off_t node_head = tree_data.block_cnt,
					node_rear = tree_data.block_cnt + 1;

				tree_data.head = node_head;
				tree_data.rear = node_rear;

				create_leaf(0, 0, node_rear);
				create_leaf(0, node_head, 0);

				return;
			}
			char buff[BLOCK_SIZE] = { 0 };
			mem_read(buff, BLOCK_SIZE, 0);
			memcpy(&tree_data, buff, sizeof(tree_data));
		}
		BTree(const BTree& other) {
			// Todo Copy
			fp = fopen(BPTREE_ADDRESS, "rb+");
			tree_data=other.tree_data;

		}
		BTree& operator=(const BTree& other) {
			// Todo Assignment
			fp = fopen(BPTREE_ADDRESS, "rb+");
			tree_data=other.tree_data;
			return *this;
		}
		~BTree() {
			// Todo Destructor
			fclose(fp);
		}
		// Insert: Insert certain Key-Value into the database
		// Return a pair, the first of the pair is the iterator point to the new
		// element, the second of the pair is Success if it is successfully inserted
		pair<iterator, OperationResult> insert(const Key& key, const Value& value) {
			// TODO insert function
			check_file();
			if (empty()) {
				off_t root_pos = create_leaf(0, tree_data.head, tree_data.rear);

				Block_Data temp_info;
				Leaf_Data temp_data;
				read_leaf(&temp_info, &temp_data, tree_data.head);
				temp_info.next = root_pos;
				write_leaf(&temp_info, &temp_data, tree_data.head);

				read_leaf(&temp_info, &temp_data, tree_data.rear);
				temp_info.last = root_pos;
				write_leaf(&temp_info, &temp_data, tree_data.rear);

				read_leaf(&temp_info, &temp_data, root_pos);
				++temp_info.num;
				temp_data.val[0].key = key;
				temp_data.val[0].value = value;
				write_leaf(&temp_info, &temp_data, root_pos);

				++tree_data.num;
				tree_data.root_pos = root_pos;
				write_tree_data();

				pair<iterator, OperationResult> result(begin(), Success);
				return result;
			}

			char buff[BLOCK_SIZE] = { 0 };
			off_t cur_pos = tree_data.root_pos, cur_parent = 0;
			while (true) {
				mem_read(buff, BLOCK_SIZE, cur_pos);
				Block_Data temp;
				memcpy(&temp, buff, sizeof(temp));

				if (cur_parent != temp.parent) {
					temp.parent = cur_parent;
					memcpy(buff, &temp, sizeof(temp));
					mem_write(buff, BLOCK_SIZE, cur_pos);
				}
				//std::cout<<temp.block_type;
				if (temp.block_type) {
					break;
				}
				Node_Data normal_data;
				memcpy(&normal_data, buff + HEAD_SIZE, sizeof(normal_data));
				off_t child_pos = temp.num - 1;
				while (child_pos > 0) {
					if (!(normal_data.val[child_pos - 1].key > key) )break;
					--child_pos;
				}
				cur_parent = cur_pos;
				cur_pos = normal_data.val[child_pos].child;
				cur_pos = cur_pos;
			}

			Block_Data info;
			memcpy(&info, buff, sizeof(info));
			Leaf_Data leaf_data;
			memcpy(&leaf_data, buff + HEAD_SIZE, sizeof(leaf_data));
			for (off_t value_pos = 0;; ++value_pos) {
				if (value_pos < info.num && (!(leaf_data.val[value_pos].key < key || leaf_data.val[value_pos].key > key))) {
                    return pair<iterator, OperationResult>(end(), Fail);
				}
				if (value_pos >= info.num || leaf_data.val[value_pos].key > key) {
					if (info.num >= L) {
						auto cur_key = split_leaf_node(cur_pos, info, leaf_data);
						if (key > cur_key) {
							cur_pos = info.next;
							value_pos -= info.num;
							read_leaf(&info, &leaf_data, cur_pos);
						}
					}

					for (off_t p = info.num - 1; p >= value_pos; --p)
					{
						leaf_data.val[p + 1]= leaf_data.val[p];
						if (p == value_pos)
							break;
					}
					leaf_data.val[value_pos].key = key;
					leaf_data.val[value_pos].value = value;
					++info.num;
					write_leaf(&info, &leaf_data, cur_pos);
					iterator ans;
					ans.block_info = info;
					ans.cur_bptree = this;
					ans.cur_pos = value_pos;
					//修改树的基本参数
					++tree_data.num;
					write_tree_data();
					pair<iterator, OperationResult> re(ans, Success);
					return re;
				}
			}
			return pair<iterator, OperationResult>(end(), Fail);
		}
		// Erase: Erase the Key-Value
		// Return Success if it is successfully erased
		// Return Fail if the key doesn't exist in the database
		OperationResult erase(const Key& key) {
			return Fail;
		}
		iterator begin() {
			check_file();
			iterator result;
			char buff[BLOCK_SIZE] = { 0 };
			mem_read(buff, BLOCK_SIZE, tree_data.head);
			Block_Data block_head;
			memcpy(&block_head, buff, sizeof(block_head));
			result.block_info = block_head;
			result.cur_bptree = this;
			result.cur_pos = 0;
			++result;
			return result;
		}
		const_iterator cbegin() const {
			const_iterator result;
			char buff[BLOCK_SIZE] = { 0 };
			mem_read(buff, BLOCK_SIZE, tree_data.head);
			Block_Data block_head;
			memcpy(&block_head, buff, sizeof(block_head));
			result.block_info = block_head;
			result.cur_pos = 0;
			++result;
			return result;
		}
		// Return a iterator to the end(the next element after the last)
		iterator end() {
			check_file();
			iterator result;
			char buff[BLOCK_SIZE] = { 0 };
			mem_read(buff, BLOCK_SIZE, tree_data.rear);
			Block_Data block_head;
			memcpy(&block_head, buff, sizeof(block_head));
			result.block_info = block_head;
			result.cur_bptree = this;
			result.cur_pos = 0;
			return result;
		}
		const_iterator cend() const {
			const_iterator result;
			char buff[BLOCK_SIZE] = { 0 };
			mem_read(buff, BLOCK_SIZE, tree_data.rear);
			Block_Data block_head;
			memcpy(&block_head, buff, sizeof(block_head));
			result.block_info = block_head;
			result.cur_pos = 0;
			return result;
		}
		// Check whether this BTree is empty
		bool empty() const {
			if (!fp)
				return true;
			return tree_data.num == 0;
		}
		// Return the number of <K,V> pairs
		off_t size() const {
			if (!fp)
				return 0;
			return tree_data.num;
		}
		// Clear the BTree
		void clear() {
			if (!fp)
				return;
			remove(BPTREE_ADDRESS);
			File_Data new_data;
			tree_data = new_data;
			fp = nullptr;
		}
		// Return the value refer to the Key(key)
		Value at(const Key& key) {
			if (empty()) {
				throw container_is_empty();
			}

			char buff[BLOCK_SIZE] = { 0 };
			off_t cur_pos = tree_data.root_pos, cur_parent = 0;
			while (true) {
				mem_read(buff, BLOCK_SIZE, cur_pos);
				Block_Data temp;
				memcpy(&temp, buff, sizeof(temp));

				if (cur_parent != temp.parent) {
					temp.parent = cur_parent;
					memcpy(buff, &temp, sizeof(temp));
					mem_write(buff, BLOCK_SIZE, cur_pos);
				}
				if (temp.block_type) break;

				Node_Data normal_data;
				memcpy(&normal_data, buff + HEAD_SIZE, sizeof(normal_data));
				off_t child_pos = temp.num - 1;
				for (; child_pos > 0; --child_pos) {
					if (!(normal_data.val[child_pos - 1].key > key)) {
						break;
					}
				}
				cur_pos = normal_data.val[child_pos].child;
			}
			Block_Data info;
			memcpy(&info, buff, sizeof(info));
			Leaf_Data leaf_data;
			memcpy(&leaf_data, buff + HEAD_SIZE, sizeof(leaf_data));
			for (off_t value_pos = 0;; ++value_pos) {
				if (value_pos < info.num && (!(leaf_data.val[value_pos].key<key || leaf_data.val[value_pos].key>key))) {
					return leaf_data.val[value_pos].value;
				}
				if (value_pos >= info.num || leaf_data.val[value_pos].key > key) {
					throw index_out_of_bound();
				}
			}
		}

		/**
		 * Returns the number of elements with key
		 *   that compares equivalent to the specified argument,
		 * The default method of check the equivalence is !(a < b || b > a)
		 */
		off_t count(const Key& key) const {
			return find(key) == cend() ? 0 : 1;
		}

		/**
		 * Finds an element with key equivalent to key.
		 * key value of the element to search for.
		 * Iterator to an element with key equivalent to key.
		 *   If no such element is found, past-the-end (see end()) iterator is
		 * returned.
		 */
		iterator find(const Key& key) {
			if (empty()) {
				return end();
			}

			char buff[BLOCK_SIZE] = { 0 };
			off_t cur_pos = tree_data.root_pos, cur_parent = 0;
			while (true) {
				mem_read(buff, BLOCK_SIZE, cur_pos);
				Block_Data temp;
				memcpy(&temp, buff, sizeof(temp));

				if (cur_parent != temp.parent) {
					temp.parent = cur_parent;
					memcpy(buff, &temp, sizeof(temp));
					mem_write(buff, BLOCK_SIZE, cur_pos);
				}
				if (temp.block_type) {
					break;
				}
				Node_Data normal_data;
				memcpy(&normal_data, buff + HEAD_SIZE, sizeof(normal_data));
				off_t child_pos = temp.num - 1;
				for (; child_pos > 0; --child_pos) {
					if (!(normal_data.val[child_pos - 1].key > key)) {
						break;
					}
				}
				cur_pos = normal_data.val[child_pos].child;
			}
			Block_Data info;
			memcpy(&info, buff, sizeof(info));
			sizeof(Node_Data);
			Leaf_Data leaf_data;
			memcpy(&leaf_data, buff + HEAD_SIZE, sizeof(leaf_data));
			for (off_t value_pos = 0;; ++value_pos) {
				if (value_pos < info.num && (!(leaf_data.val[value_pos].first<key || leaf_data.val[value_pos].first>key))) {
					iterator result;
					result.cur_bptree = this;
					result.block_info = info;
					result.cur_pos = value_pos;
					return result;
				}
				if (value_pos >= info.num || leaf_data.val[value_pos].first > key) {
					return end();
				}
			}
			return end();
		}
		const_iterator find(const Key& key) const {
			if (empty()) {
				return cend();
			}

			char buff[BLOCK_SIZE] = { 0 };
			off_t cur_pos = tree_data.root_pos, cur_parent = 0;
			while (true) {
				mem_read(buff, BLOCK_SIZE, cur_pos);
				Block_Data temp;
				memcpy(&temp, buff, sizeof(temp));
				
				if (cur_parent != temp.parent) {
					temp.parent = cur_parent;
					memcpy(buff, &temp, sizeof(temp));
					mem_write(buff, BLOCK_SIZE, cur_pos);
				}
				if (temp.block_type) {
					break;
				}
				Node_Data normal_data;
				memcpy(&normal_data, buff + HEAD_SIZE, sizeof(normal_data));
				off_t child_pos = temp.num - 1;
				for (; child_pos > 0; --child_pos) {
					if (!(normal_data.val[child_pos - 1].key > key)) {
						break;
					}
				}
				cur_pos = normal_data.val[child_pos].child;
			}
			Block_Data info;
			memcpy(&info, buff, sizeof(info));
			Leaf_Data leaf_data;
			memcpy(&leaf_data, buff + HEAD_SIZE, sizeof(leaf_data));
			for (off_t value_pos = 0;; ++value_pos) {
				if (value_pos < info.num && (!(leaf_data.val[value_pos].key<key || leaf_data.val[value_pos].key>key))) {
					const_iterator result;
					result.block_info = info;
					result.cur_pos = value_pos;
					return result;
				}
				if (value_pos >= info.num || leaf_data.val[value_pos].key > key) {
					return cend();
				}
			}
			return cend();
		}
	};
	template <typename Key, typename Value, typename Compare> FILE* BTree<Key, Value, Compare>::fp = nullptr;
}  // namespace sjtu
