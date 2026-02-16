import 'dart:async';
import 'dart:convert';
import 'package:flutter/material.dart';
import 'package:http/http.dart' as http;
import 'package:provider/provider.dart' show Provider, ChangeNotifierProvider;
import '../utils/helpers.dart';
export 'package:flutter/widgets.dart';
export 'src/models.dart' show User, Profile;

/// Typedef for a JSON map.
typedef JsonMap = Map<String, dynamic>;

/// Typedef for a callback.
typedef OnPressed = void Function();

/// Application theme constants.
const String appName = 'MyApp';
final double defaultPadding = 16.0;
var int requestCount = 0;

/// Mixin for validation logic.
mixin ValidationMixin {
  bool isValidEmail(String email) {
    return email.contains('@');
  }
}

/// Extension on String for convenience methods.
extension StringExtension on String {
  String capitalize() {
    if (isEmpty) return this;
    return '${this[0].toUpperCase()}${substring(1)}';
  }
}

/// Enum for user roles.
enum UserRole {
  admin,
  editor,
  viewer,
}

/// Abstract base class for repositories.
abstract class Repository<T> {
  Future<T?> findById(String id);
  Future<List<T>> findAll();
  Future<void> save(T entity);
  Future<void> delete(String id);
}

/// Data model for a user.
class User {
  final String id;
  final String name;
  final String email;
  final UserRole role;

  const User({
    required this.id,
    required this.name,
    required this.email,
    this.role = UserRole.viewer,
  });

  factory User.fromJson(JsonMap json) {
    return User(
      id: json['id'] as String,
      name: json['name'] as String,
      email: json['email'] as String,
    );
  }

  JsonMap toJson() => {
        'id': id,
        'name': name,
        'email': email,
      };
}

/// Service for API calls.
class ApiService with ValidationMixin {
  final http.Client _client;
  final String _baseUrl;

  ApiService(this._baseUrl) : _client = http.Client();

  Future<User> fetchUser(String id) async {
    final response = await _client.get(Uri.parse('$_baseUrl/users/$id'));
    if (response.statusCode == 200) {
      return User.fromJson(jsonDecode(response.body));
    }
    throw Exception('Failed to fetch user');
  }
}

/// Private helper class (should not be exported).
class _InternalCache {
  final Map<String, dynamic> _store = {};

  void set(String key, dynamic value) {
    _store[key] = value;
  }
}

/// A Flutter widget.
class UserProfileWidget extends StatefulWidget {
  final User user;

  const UserProfileWidget({super.key, required this.user});

  @override
  State<UserProfileWidget> createState() => _UserProfileWidgetState();
}

class _UserProfileWidgetState extends State<UserProfileWidget> {
  @override
  Widget build(BuildContext context) {
    return Text(widget.user.name);
  }
}

/// Top-level function.
void initializeApp() {
  // startup logic
}

/// Another top-level function.
Future<void> loadConfiguration(String path) async {
  // load config
}

/// Private top-level function.
void _setupLogging() {
  // internal setup
}
